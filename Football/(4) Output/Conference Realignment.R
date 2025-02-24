library(dplyr)
library(readxl)
library(caret)
library(openxlsx)
### Read in data
cfb.data<-read_xlsx("./Conference Realignment Investigation.xlsx", sheet = 1)
cbb.data<-read_xlsx("./Conference Realignment Investigation.xlsx", sheet = 2)

## join by school
names(cfb.data)[-c(1, ncol(cfb.data))]<-c(paste0(names(cfb.data)[-c(1, ncol(cfb.data))], "_cfb"))
names(cbb.data)[-c(1, ncol(cbb.data))]<-c(paste0(names(cbb.data)[-c(1, ncol(cbb.data))], "_cbb"))

head(cfb.data)
cfb.data<-data.frame(apply(cfb.data, 2, function(x) ifelse(is.na(x), 0, x)), check.names = F)
# Make columns numeric
cfb.data[,-c(1, ncol(cfb.data)-1, ncol(cfb.data))]<-apply(cfb.data[,-c(1, ncol(cfb.data)-1, ncol(cfb.data))], 2, function(x) as.numeric(x))
cfb.boxcox<-preProcess(cfb.data[,-c(1:3, ncol(cfb.data)-1, ncol(cfb.data))], method = "BoxCox")


head(cbb.data)
sum(is.na(cbb.data))

#full.dat<-left_join(cbb.data,cfb.data, by = "School")

## Normalize Data
# Histograms of each variable
hist(cbb.data$Yrs_cbb) 
hist(cfb.data$Yrs_cfb) #Years = Min-Max
hist(cbb.data$`W-L%_cbb`) 
hist(cfb.data$`Overall Pct_cfb`) #W-L% = Z-Score
hist(cbb.data$SRS_cbb)
hist(cfb.data$SRS_cfb) #SRS = Z-Score
hist(cbb.data$SOS_cbb)
hist(cfb.data$SOS_cfb) #SOS = Min-Max
hist(cbb.data$AP_cbb)
hist(cfb.data$AP_cfb) #AP = Box-Cox
hist(cbb.data$NCAA_cbb) #NCAA = Box-Cox
hist(cbb.data$FF_cbb) #FF = Box-Cox
hist(cfb.data$`Bowls G_cfb`)  #Bowl G - Box-Cox

## Min-Max Normalization on Years, SOS for CBB and CFB
cbb.data$Yrs_cbb_norm<-(cbb.data$Yrs_cbb - min(cbb.data$Yrs_cbb))/(max(cbb.data$Yrs_cbb) - min(cbb.data$Yrs_cbb))
cbb.data$SOS_cbb_norm<-(cbb.data$SOS_cbb - min(cbb.data$SOS_cbb))/(max(cbb.data$SOS_cbb) - min(cbb.data$SOS_cbb))
cfb.data$Yrs_cfb_norm<-(cfb.data$Yrs_cfb - min(cfb.data$Yrs_cfb))/(max(cfb.data$Yrs_cfb) - min(cfb.data$Yrs_cfb))
cfb.data$SOS_cfb_norm<-(cfb.data$SOS_cfb - min(cfb.data$SOS_cfb))/(max(cfb.data$SOS_cfb) - min(cfb.data$SOS_cfb))

## Z-Score Normalization on W-L%, SRS for CBB and CFB
cbb.data$`W-L%_cbb_norm`<-(cbb.data$`W-L%_cbb` - mean(cbb.data$`W-L%_cbb`))/sd(cbb.data$`W-L%_cbb`)
cbb.data$SRS_cbb_norm<-(cbb.data$SRS_cbb - mean(cbb.data$SRS_cbb))/sd(cbb.data$SRS_cbb)
cfb.data$`Overall Pct_cfb_norm`<-(cfb.data$`Overall Pct_cfb` - mean(cfb.data$`Overall Pct_cfb`))/sd(cfb.data$`Overall Pct_cfb`)
cfb.data$SRS_cfb_norm<-(cfb.data$SRS_cfb - mean(cfb.data$SRS_cfb))/sd(cfb.data$SRS_cfb)

## Z-score Transformation on AP, NCAA, FF, for CBB and AP, Bowl G for CFB
cbb.data$AP_cbb_norm<-(cbb.data$AP_cbb - mean(cbb.data$AP_cbb))/sd(cbb.data$AP_cbb)
cbb.data$NCAA_cbb_norm<-(cbb.data$NCAA_cbb - mean(cbb.data$NCAA_cbb))/sd(cbb.data$NCAA_cbb)
cbb.data$FF_cbb_norm<-(cbb.data$FF_cbb - mean(cbb.data$FF_cbb))/sd(cbb.data$FF_cbb)
cfb.data$AP_cfb_norm<-(cfb.data$AP_cfb - mean(cfb.data$AP_cfb))/sd(cfb.data$AP_cfb)
cfb.data$`Bowls G_cfb_norm`<-(cfb.data$`Bowls G_cfb` - mean(cfb.data$`Bowls G_cfb`))/sd(cfb.data$`Bowls G_cfb`)

# Examine preservation of shapes
plot(density(cbb.data$Yrs_cbb))
plot(density(cbb.data$Yrs_cbb_norm))
plot(density(cfb.data$Yrs_cfb))
plot(density(cfb.data$Yrs_cfb_norm))
plot(density(cbb.data$SOS_cbb))
plot(density(cbb.data$SOS_cbb_norm))  
plot(density(cfb.data$SOS_cfb))
plot(density(cfb.data$SOS_cfb_norm))
plot(density(cbb.data$`W-L%_cbb`))
plot(density(cbb.data$`W-L%_cbb_norm`))
plot(density(cfb.data$`Overall Pct_cfb`))
plot(density(cfb.data$`Overall Pct_cfb_norm`))
plot(density(cbb.data$SRS_cbb))
plot(density(cbb.data$SRS_cbb_norm))
plot(density(cfb.data$SRS_cfb))
plot(density(cfb.data$SRS_cfb_norm))
plot(density(cbb.data$AP_cbb))
plot(density(cbb.data$AP_cbb_norm))
plot(density(cfb.data$AP_cfb))
plot(density(cfb.data$AP_cfb_norm))
plot(density(cbb.data$NCAA_cbb))
plot(density(cbb.data$NCAA_cbb_norm))
plot(density(cbb.data$FF_cbb))
plot(density(cbb.data$FF_cbb_norm))

## Combine data
full.dat<-left_join(cbb.data,cfb.data, by = c("School", "Region"))

# Binary indicators for region
full.dat$Region<-as.factor(full.dat$Region)
full.dat$Northeast<-ifelse(full.dat$Region == "Northeast", 1, 0)
full.dat$Midwest<-ifelse(full.dat$Region == "Midwest", 1, 0)
full.dat$Southwest<-ifelse(full.dat$Region == "Southwest", 1, 0)
full.dat$West<-ifelse(full.dat$Region == "West", 1, 0)
full.dat$Southeast<-ifelse(full.dat$Region == "Southeast", 1, 0)

## K-Means
full.dat.knn<-full.dat[,-c(2:17,19, 27:43)] %>% na.omit()
full.dat.NE<-full.dat.knn[full.dat.knn$Northeast == 1,]
full.dat.MW<-full.dat.knn[full.dat.knn$Midwest == 1,]
full.dat.SW<-full.dat.knn[full.dat.knn$Southwest == 1,]
full.dat.W<-full.dat.knn[full.dat.knn$West == 1,]
full.dat.SE<-full.dat.knn[full.dat.knn$Southeast == 1,]

set.seed(88)
kmeans_NE<-kmeans(full.dat.NE[,-1], centers = 2, nstart = 15)
kmeans_MW<-kmeans(full.dat.MW[,-1], centers = 2, nstart = 15)
kmeans_SW<-kmeans(full.dat.SW[,-1], centers = 3, nstart = 15)
kmeans_W<-kmeans(full.dat.W[,-1], centers = 3, nstart = 15)
kmeans_SE<-kmeans(full.dat.SE[,-1], centers = 3, nstart = 20)

full.dat.NE$Cluster<-kmeans_NE$cluster
full.dat.MW$Cluster<-kmeans_MW$cluster+2
full.dat.SW$Cluster<-kmeans_SW$cluster+4
full.dat.W$Cluster<-kmeans_W$cluster+7
full.dat.SE$Cluster<-kmeans_SE$cluster+10

full.dat.fb<-rbind(full.dat.NE, full.dat.MW, full.dat.SW, full.dat.W, full.dat.SE)

# K-Means on Basketball-only
bb.dat.knn<-full.dat[,-c(2:7, 9:17,19, 27:43)] %>% filter(is.na(`Overall Pct_cfb_norm`))

bb.dat.NE<-bb.dat.knn[bb.dat.knn$Northeast == 1,]
bb.dat.MW<-bb.dat.knn[bb.dat.knn$Midwest == 1,]
bb.dat.SW<-bb.dat.knn[bb.dat.knn$Southwest == 1,]
bb.dat.W<-bb.dat.knn[bb.dat.knn$West == 1,]
bb.dat.SE<-bb.dat.knn[bb.dat.knn$Southeast == 1,]
set.seed(8)
kmeans_NE<-kmeans(bb.dat.NE[,-c(1, 11:17)], centers = 8, nstart = 40)
kmeans_MW<-kmeans(bb.dat.MW[,-c(1, 11:17)], centers = 3, nstart = 30)
kmeans_SW<-kmeans(bb.dat.SW[,-c(1, 11:17)], centers = 2, nstart = 30)
kmeans_W<-kmeans(bb.dat.W[,-c(1, 11:17)], centers = 2, nstart = 30)
kmeans_SE<-kmeans(bb.dat.SE[,-c(1, 11:17)], centers = 4, nstart = 30)

bb.dat.NE$Cluster<-kmeans_NE$cluster
bb.dat.MW$Cluster<-kmeans_MW$cluster+8
bb.dat.SW$Cluster<-kmeans_SW$cluster+11
bb.dat.W$Cluster<-kmeans_W$cluster+13
bb.dat.SE$Cluster<-kmeans_SE$cluster+15

bb.dat<-rbind(bb.dat.NE, bb.dat.MW, bb.dat.SW, bb.dat.W, bb.dat.SE)

bb.dat<-bb.dat[order(bb.dat$Cluster),]
full.dat.fb<-full.dat.fb[order(full.dat.fb$Cluster),]

# Export
wb<-createWorkbook()
addWorksheet(wb, "FB Data")
writeData(wb, "FB Data", full.dat.fb)
addWorksheet(wb, "BB Data")
writeData(wb, "BB Data", bb.dat)
saveWorkbook(wb, "Conference Realignment Clustering v5.xlsx", overwrite = TRUE)
