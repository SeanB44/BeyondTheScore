#### LOAD LIBRARIES ####
library(caTools)
library(ROCR)
library(dplyr)
library(ggplot2)
library(e1071)
library(caret)
library(dplyr)
library(readxl)
library(openxlsx)
library(tidyverse)
library(tidyr)
library(randomForest)
library(forecast)
library(car)

# ### Read in Data ###
pitcher.dat<-read_xlsx("./Pitcher Clustering.xlsx", sheet = "Data", )
names(pitcher.dat)<-c("Pitcher","GB/FB", "HR/FB","Pull%","Cent%",	"Oppo%",	"Soft%",	"Med%",
                      "Hard%",	"EV",	"LA",	"Barrel%",	"HardHit%",	"Strike/Ball%",
                      "FB_Vert",	"FB_Horz",	"O-Swing%",	"Z-Swing%",	"Swing%",	"O-Contact%",	"Z-Contact%",
                      "Contact%",	"Zone%",	"Pace",	"vFA",	"FB%")
pitcher.dat<-na.omit(pitcher.dat)
str(pitcher.dat)

pitcher.dat$EV<-as.numeric(pitcher.dat$EV)
pitcher.dat$LA<-as.numeric(pitcher.dat$LA)
pitcher.dat$`Barrel%`<-as.numeric(gsub("%", "",pitcher.dat$`Barrel%`))
pitcher.dat$`HardHit%`<-as.numeric(gsub("%", "",pitcher.dat$`HardHit%`))

## Build Dendrogram to find optimal # of clusters
dendrogram<-hclust(dist(pitcher.dat[,-1], method = "euclidean"), method = 'ward.D')
plot(dendrogram,
     main = paste("Dendrogram"),
     xlab = "Pitchers", ylab = "Euclidean Distance")
## Fit Hierarchical cluster to dataset
hc<-hclust(dist(pitcher.dat[,-1], method = "euclidean"), method = 'ward.D')
y_hc<-cutree(hc, k=3)

## Vizualize
library(cluster)
# clusplot(pitcher.dat[,-1], y_hc, lines=0, shade=TRUE,
#          color=TRUE, labels=5, plotchar=FALSE, span = TRUE,
#          main = paste("2024 Pitcher Clustering"), xlab="X",
#          ylab = "Y")
hc$order
hc$height

#Using the elbow method to find optimal # of clusters
set.seed(44)
#Use for loop
wcss<-vector()
scale(pitcher.dat[,-1])
for (i in 1:10) wcss[i]<-sum(kmeans(scale(pitcher.dat[,-1]),i)$withinss)
plot(1:10,wcss,type = "b", main = paste("Clusters of Pitchers"),xlab = "Number of Clusters", ylab = "WCSS")
#Optimal # of Clusters is 4
#Fit K-Means to Pitcher with right number of clusters (4)

#Apply K-Means to Mall Dataset
set.seed(29)
kmeans #Performs kmeans clustering on a dataset
kmeans<-kmeans(scale(pitcher.dat[,-1]), 4, iter.max = 300, nstart = 10)
#Vizualizing Clusters
library(cluster)
?clusplot
summary(kmeans)
kmeans$cluster
kmeans$centers
?clusplot(pitcher.dat[,-1], kmeans$cluster, lines=0, shade=TRUE,
         color=TRUE, labels=2, plotchar=FALSE, span = TRUE,
         main = paste("Clusters of Pitchers"), xlab="GB/FB",
         ylab = "LD%")
View(kmeans)

  