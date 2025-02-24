library(dplyr)
library(readxl)
library(caret)
library(openxlsx)
### Read in data
ws.dat<-read_xlsx("./Data/Standings Analysis.xlsx", sheet = "World Series Champions")
head(ws.dat)

## Filter for necessary variables
ws.dat<-ws.dat %>% 
  # extract last 4 characters from Index
  mutate(Year = as.numeric(substr(Index, nchar(Index)-3, nchar(Index))))%>%
  mutate(OPS = OBP + SLG)%>%
  select(Year, Team, 
         `Win%`, 
         `K/9`,`BB/9`,`HR/9`,`LOB%`,ERA, FIP, `P-WAR/162`,
          HR, `SB`, `AVG`, `BB%`, `K%`, `OBP`, `SLG`, `wOBA`, `wRC+`, `B-WAR/162`,
         `BsR`, `Def`) %>%
  rename(`Pitching WAR` = `P-WAR/162`, `Batting WAR` = `B-WAR/162`)

# Variable to remove
cor.matrix<-cor(ws.dat[,-c(1,2,3)]) # No variables are highly correlated
findCorrelation(cor.matrix, cutoff = 0.7) # No variables are highly correlated)         
colnames(cor.matrix)[c(15,13, 10,5,12,1,17,3)]
cor.matrix[,15] #wOBA and OBP, AVG, SLG
cor.matrix[,13] #OBP and AVG, wOBA,  BB% 
cor.matrix[,10] #AVG and LOB%, wOBA, OBP
cor.matrix[,5] #ERA and FIP
cor.matrix[,12] #K% and K/9, HR/9
cor.matrix[,1] #B-WAR and wRC+
cor.matrix[,17] #wRC+ and wOBA, SLG
cor.matrix[,1] #K/9 and HR/9, K%
cor.matrix[,3] #HR/9 and HR, K%, K/9

## Remove variables
ws.dat$AVG<-NULL
ws.dat$OBP<-NULL
ws.dat$`K%`<-NULL
ws.dat$`HR/9`<-NULL
ws.dat$`LOB%`<-NULL

## Normalize Data
# Histograms of each variable

hist(ws.dat$`Win%`) 
#hist(ws.dat$`K/9`) 
hist(ws.dat$`BB/9`) 
#hist(ws.dat$`HR/9`) 
#hist(ws.dat$`LOB%`) 
hist(ws.dat$ERA)
hist(ws.dat$FIP)
hist(ws.dat$`Pitching WAR`)
hist(ws.dat$HR)
hist(ws.dat$`SB`)
#hist(ws.dat$`AVG`)
hist(ws.dat$`BB%`)
#hist(ws.dat$`K%`)
#hist(ws.dat$OBP)
hist(ws.dat$SLG)
hist(ws.dat$wOBA)
hist(ws.dat$`wRC+`)
hist(ws.dat$`Batting WAR`)
hist(ws.dat$BsR)
hist(ws.dat$Def)

# Most distributions are fairly normal.  Z-Score should be fine for normalization

## Z-Score Normalization of Predictor Variables
ws.norm.dat<-ws.dat
ws.norm.dat[,-c(1:2)]<-apply(ws.dat[,-c(1,2)], 2, function(x) (x - mean(x))/sd(x))

# # Examine preservation of shapes
# plot(density(ws.dat$`Win%`))
# plot(density(ws.norm.dat$`Win%`))
# plot(density(ws.dat$`K/9`))
# plot(density(ws.norm.dat$`K/9`))
# plot(density(ws.dat$`BB/9`))
# plot(density(ws.norm.dat$`BB/9`))
# plot(density(ws.dat$`HR/9`))
# plot(density(ws.norm.dat$`HR/9`))
# plot(density(ws.dat$`LOB%`))
# plot(density(ws.norm.dat$`LOB%`))
# plot(density(ws.dat$ERA))
# plot(density(ws.norm.dat$ERA))
# plot(density(ws.dat$FIP))
# plot(density(ws.norm.dat$FIP))
# plot(density(ws.dat$`Pitching WAR`))
# plot(density(ws.norm.dat$`Pitching WAR`))
# plot(density(ws.dat$HR))
# plot(density(ws.norm.dat$HR))
# plot(density(ws.dat$`SB`))
# plot(density(ws.norm.dat$`SB`))
# plot(density(ws.dat$`AVG`))
# plot(density(ws.norm.dat$`AVG`))
# plot(density(ws.dat$`BB%`))
# plot(density(ws.norm.dat$`BB%`))
# 
# ## KNN
# KNN on full dataset
set.seed(8)
km.out <- kmeans(ws.norm.dat[,-c(1:2)], centers = 3, nstart = 25)
km.out

# Decide how many clusters to look at
n_clusters <- 10

# Initialize total within sum of squares error: wss
wss <- numeric(n_clusters)

set.seed(3)

# Look over 1 to n possible clusters
for (i in 1:n_clusters) {
  # Fit the model: km.out
  km.out <- kmeans(ws.norm.dat[,-c(1:2)], centers = i, nstart = 20)
  # Save the within cluster sum of squares
  wss[i] <- km.out$tot.withinss
}

# Produce a scree plot
wss_df <- tibble(clusters = 1:n_clusters, wss = wss)

scree_plot <- ggplot(wss_df, aes(x = clusters, y = wss, group = 1)) +
  geom_point(size = 4)+
  geom_line() +
  scale_x_continuous(breaks = c(2, 4, 6, 8, 10)) +
  xlab('Number of clusters')
scree_plot

scree_plot +
  geom_hline(
    yintercept = wss, 
    linetype = 'dashed', 
    col = c(rep('#000000',4),'#FF0000', rep('#000000', 5))
  )
# Going with 5 clusters
set.seed(3)
km.out <- kmeans(ws.norm.dat[,-c(1:2)], centers = 5, nstart = 25)
km.out

# Calculate euclidean distance between the first point and each other point in the dataset
# Combine column 2 and column 1 of ws.dat to get unique name
nms<-c()
for(i in 1:nrow(ws.dat)){
  nms[i]<-paste0(ws.dat$Team[i], " - ", ws.dat$Year[i])
}
nms

ws.dat$Cluster<-km.out$cluster
ws.norm.dat$Cluster<-km.out$cluster

(distances <- as.matrix(dist(ws.norm.dat[,-c(1:2)], method = 'euclidean', diag = FALSE)))
# Make 0s into NAs
distances[distances == 0] <- NA
distances.df<-as.data.frame(distances)
colnames(distances.df)<-nms
rownames(distances.df)<-nms
distances.df

# Export distances
wb<-createWorkbook()
addWorksheet(wb, "Distances")
writeData(wb, "Distances", distances.df, rowNames = TRUE, colNames = TRUE)
saveWorkbook(wb, file = "./Output/World Series Champion Similarity Scores.xlsx", overwrite = TRUE)

# Find closest distance to each point
closest<-c()
for(i in 1:nrow(distances.df)){
  closest[i]<-rownames(distances.df)[which.min(distances.df[i,])]
}
closest

ws.dat$`Most Similar Team`<-closest
ws.norm.dat$`Most Similar Team`<-closest

#  New data frame sorted by clusters
clusterd.ws.dat<-ws.dat %>%
  arrange(Cluster)

# Save cluster data
cluster.dat<-km.out$centers

# find the mean of each variable for each cluster
cluster.stats.dat<-apply(ws.dat[,-c(1:2, ncol(ws.dat)-1, ncol(ws.dat))], 2, function(x) tapply(x, ws.dat$Cluster, mean))

# Export to workbook
wb<-createWorkbook()
addWorksheet(wb, "Full Data")
writeData(wb, "Full Data", ws.dat)
addWorksheet(wb, "Normalized Data")
writeData(wb, "Normalized Data", ws.norm.dat)
addWorksheet(wb, "Clustered Data")
writeData(wb, "Clustered Data", clusterd.ws.dat)
addWorksheet(wb, "Cluster Centers")
writeData(wb, "Cluster Centers", cluster.dat)
addWorksheet(wb, "Cluster Stats")
writeData(wb, "Cluster Stats", cluster.stats.dat)
#
saveWorkbook(wb, file = "./Output/World Series Clustering.xlsx", overwrite = TRUE)
