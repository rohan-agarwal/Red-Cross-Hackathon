#----init----

#for SQL queries
library(sqldf)

#for data visualization
library(ggplot2)

#tree
library(tree)

#neural network
library(nnet)

#boosted tree
library(gbm)

#random forest
library(randomForest)

#clustering
library(fpc)
library(flexclust)

#cross validation
library(cvTools)

#mapping
library(maptools)
library(grid)

#input and format all datasets
#demographic info
demo <- read.csv("demo.csv")
demo <- demo[,-1]
names(demo) <- c("ZipCode", "Income10k", "Income15k", "Income25k", "Income35k", 
                 "Income50k", "Income75k", "Income99k", "Income150k", 
                 "Income200k", "HighIncome", "Asian", "Black",
                 "Hispanic", "White", "Other", "HS", "Bach", "Married",
                 "NeverMarried", "Poverty", "Unemployed", "Male", 
                 "Female", "Population")

#zip code info
zip <- read.csv("zip.csv")
names(zip) <- c("Name", "ZipCode")

#volunteer roles
resp <- read.csv("resp.csv")
resp$role <- as.factor(resp$role)
resp <- resp[resp$role!="",]
resp <- resp[resp$role!="wrong_number",]
resp$role <- as.factor(as.character(resp$role))
names(resp) <- c("IncidentID","Name","Role")

#fire data
fire <- read.csv("fire.csv")
names(fire)[2] <- "IncidentID"
names(fire)[45] <- "name2"

#create backups
demo_backup <- demo
zip_backup <- zip
resp_backup <- resp
fire_backup <- fire

multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  require(grid)
  
  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)
  
  numPlots = length(plots)
  
  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                     ncol = cols, nrow = ceiling(numPlots/cols))
  }
  
  if (numPlots==1) {
    print(plots[[1]])
    
  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
    
    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
      
      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}

#----Volunteer analysis----

resp$Responded <- 1
resp$Responded[resp$Role == "no_answer"] = 0
resp$Responded[resp$Role == "no_longer_active"] = 0
resp$Responded[resp$Role == "not_available"] = 0
resp$Responded[resp$Role == "team_lead"] = 2

vol <- merge(zip, resp, by="Name")

vol <- sqldf("SELECT Name, AVG(Responded) AS Score, 
             ZipCode FROM vol GROUP BY Name")
vol <- sqldf("SELECT ZipCode, SUM(score)*AVG(Score) AS Score 
             FROM vol GROUP BY ZipCode")

vol <- merge(vol, demo, by="ZipCode")

#omit outlier containing the red cross hq
vol <- vol[vol$ZipCode!=60612,]
#clean
vol <- na.omit(vol)

#pca
vol.pca <- prcomp(vol[,-c(1:2)], center = TRUE, scale. = FALSE)
vol.pred <- predict(vol.pca, vol)
vol.pred <- data.frame(vol.pred)

qplot(data = vol.pred, x=PC1, y=PC2, color=vol$Score, 
      size=vol$Score) + scale_color_gradient(low="blue", high="red")

#k-means
#using Jaccard similarity to find if clustering is appropriate
clusterboot(vol[,-1], B=20, bootmethod="boot",
            clustermethod=kmeansCBI,
            krange=3, seed=15555)

#optimal number of clusters
wcss <- vector()

for (i in 2:30) {
  wcss[i-1] <- sum(kmeans(vol[,-1], centers=i)$withinss)
}
plot(2:30,wcss,type="b", xlab="Number of Clusters", ylab="WCSS")

#execute k-means with k=5 (based on silhouette & Jaccard)
vol.clusters <- kcca(vol[,-1], k=5, kccaFamily("kmeans"))

vol <- cbind(vol, "Cluster"=predict(vol.clusters))
parameters(vol.clusters)

vol$Cluster <- as.factor(vol$Cluster)
levels(vol$Cluster) <- 
  rank(data.frame(parameters(vol.clusters))$Score)

#predict for new data
volPred <- cbind(demo, "Cluster" = predict(vol.clusters, demo))
volPred$Cluster <- as.factor(volPred$Cluster)
levels(volPred$Cluster) <- 
  rank(data.frame(parameters(vol.clusters))$Score)
volPred$Cluster <- as.numeric(as.character(volPred$Cluster))
#volPred <- volPred[!(volPred$ZipCode %in% vol$ZipCode),]
write.csv(volPred, "volPred.csv")

#----Fire analysis----

#preparation
fireList <- sqldf("SELECT zip AS ZipCode, Count(*) AS FireCount FROM fire
              GROUP BY zip")
fireList <- na.omit(fireList)
fireList <- merge(fireList, demo, by="ZipCode")

#PCA and visualization
fireList <- na.omit(fireList)
fire.pca <- prcomp(fireList[,-c(1:2)], center = TRUE, scale. = FALSE)
fire.pred <- predict(fire.pca, fireList)
fire.pred <- data.frame(fire.pred)

qplot(data = fire.pred, x=PC1, y=PC2, color=fireList$FireCount, 
      size=fireList$FireCount) + scale_color_gradient(low="blue", high="red")

#scale before modeling
fireList[,2:26] <- scale(fireList[,2:26])

#boosted tree
boostedtree <- gbm(FireCount~.,data=fireList[,-1],
                   var.monotone=rep(0,ncol(fireList)-2),
                   distribution="gaussian", n.trees=1200, shrinkage=.1, 
                   interaction.depth=3, bag.fraction = .5, train.fraction = 1, 
                   n.minobsinnode = 10, cv.folds= 10, 
                   keep.data=TRUE, verbose=FALSE)
best.iter <- gbm.perf(boostedtree,method="cv");best.iter
sqrt(boostedtree$cv.error[best.iter])
summary(boostedtree,n.trees=best.iter)
boostedtree.pred <- predict(boostedtree)
e <- fireList$FireCount - boostedtree.pred
1 - var(e)/var(fireList$FireCount)

#random forest
fire.rf <- randomForest(FireCount~.,data=fireList[,-1])
fire.rf.pred <- predict(fire.rf)
e <- fireList$FireCount - fire.rf.pred
1 - var(e)/var(fireList$FireCount)

#neural network
fire.nn <- nnet(FireCount~.,fireList[,-1], linout=T, 
                skip=F, size=5, decay=2, maxit=200, trace=F)
fire.nn.pred <- predict(fire.nn)
e <- fireList$FireCount - fire.nn.pred
1 - var(e)/var(fireList$FireCount)

#decision tree
control <- tree.control(nobs=nrow(fireList),
                        mincut = 5, minsize = 15, mindev = .002)
fire.tr <- tree(FireCount~.,fireList[,-1],control=control)
plot(fire.tr,type="u"); text(fire.tr,digits=2)
fire.tr.pred <- predict(fire.tr)
e <- fireList$FireCount - fire.tr.pred
1 - var(e)/var(fireList$FireCount)

#cross validation
folds <- cvFolds(n = nrow(fireList), K = 10, R = 1)
cvFit(boostedtree,data=fireList[,-1],
      x=fireList[,-c(1,2)],y=fireList$FireCount,folds = folds)
cvFit(fire.rf,data=fireList[,-1],
      x=fireList[,-c(1,2)],y=fireList$FireCount,folds = folds)
cvFit(fire.nn,data=fireList[,-1],
      x=fireList[,-c(1,2)],y=fireList$FireCount,folds = folds)
cvFit(fire.tr,data=fireList[,-1],
      x=fireList[,-c(1,2)],y=fireList$FireCount,folds = folds)

#decision tree is the best - final model
#restore non-standardized values
fireList <- sqldf("SELECT zip AS ZipCode, Count(*) AS FireCount FROM fire
              GROUP BY zip")
fireList <- na.omit(fireList)
fireList <- merge(fireList, demo, by="ZipCode")
fireList <- na.omit(fireList)

#create decision tree
control <- tree.control(nobs=nrow(fireList),
                        mincut = 5, minsize = 15, mindev = .002)
fire.tr <- tree(FireCount~.,fireList[,-1],control=control)
plot(fire.tr,type="u"); text(fire.tr,digits=2)
fire.tr.pred <- predict(fire.tr)
e <- fireList$FireCount - fire.tr.pred
1 - var(e)/var(fireList$FireCount)

#predict on full demographic set
firePred <- cbind(demo, "FirePred" = predict(fire.tr, demo))
#firePred <- firePred[!(firePred$ZipCode %in% fireList$ZipCode),]
write.csv(firePred, "firePred.csv")

#----Visualization----
zipshp <- readShapeSpatial("cb_2013_us_zcta510_500k.shp")
shpbackup <- zipshp
zipshp <- zipshp[zipshp$ZCTA5CE10 %in% demo$ZipCode,]
plot(zipshp)

volPred <- read.csv("volPred.csv")
volPred <- volPred[,-1]

firePred <- read.csv("firePred.csv")
firePred <- firePred[,-1]

nearby <- read.csv("nearby.csv")

zipshp <- fortify(zipshp, region = "ZCTA5CE10")
zipshp <- zipshp[zipshp$id %in% nearby$ZipCode,]

p1 <- ggplot() + geom_map(data = volPred, map = zipshp, 
                    aes(map_id = ZipCode, fill = Cluster,
                        title = "Volunteer Clusters")) + 
  expand_limits(x = zipshp$long, y = zipshp$lat) +
  scale_fill_gradient(low="gray", high="#0fae00")

p2 <- ggplot() + geom_map(data = firePred, map = zipshp, 
                    aes(map_id = ZipCode, fill = FirePred,
                        title= "Predicted Number of Fires")) + 
  expand_limits(x = zipshp$long, y = zipshp$lat) +
  scale_fill_gradient(low="gray", high="#7e1314")

multiplot(p1,p2, cols=2)
