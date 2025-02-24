## Libraries ##
library(dplyr)
library(ggplot2)
library(e1071)
library(caret)
library(XML)
library(readxl)
library(tidyr)
library(tidyverse)
library(openxlsx)
library(hoopR)
library(RCurl)
library(rvest)
library(ISLR2)
library(leaps)
library(car)
library(pls)
library(relaimpo)
library(forecast)
library(Boruta)
library(randomForest)
library(toRvik)
library(cbbdata)

source("../../Codes.R")
source("../../Functions.R")
login(user_email = Sys.getenv("KP_USER"), user_pw = Sys.getenv("KP_PW"))
cbbdata::cbd_login(username = Sys.getenv('CBD_USER'), password = Sys.getenv('CBD_PW'))

## Read in Data
upset.dat <- read.xlsx("./Data/Tournament Matchups.xlsx", sheet = "Upsets (4+ Seed Diff)")
stats.dat <- read.xlsx("./Data/Tournament Matchups.xlsx", sheet = "Tournament Matchups")

# ## Filter Data
# stats.dat<-stats.dat %>% filter(SEED.DIFFERENCE >= 4)
# names(stats.dat)

## Select data for Underdogs and Favorites by removing columns respective to Favorites/Underdogs or Differences
underdog.dat<- stats.dat %>% 
  rename(HIGH.SEED = FAVORITE.SEED) %>%
  rename(SEED.DIFF = SEED.DIFFERENCE) %>%
  dplyr::select(-c(matches("FAVORITE."), matches("DIFFERENCE.")))
head(underdog.dat)

## Filter to get games of 4+ seed difference
underdog.dat<-underdog.dat %>% filter(abs(SEED.DIFF) >= 4)

## Clean Data
# Check for NAs
colSums(is.na(underdog.dat))
# Big Upset as factor
underdog.dat$BIG.UPSET<-as.factor(underdog.dat$BIG.UPSET)
# Remove unneeded columns
colnames(underdog.dat)
underdog.dat.1<-underdog.dat %>% dplyr::select(-c("BY.YEAR.NO", "BY.ROUND.NO"))

## Run 1
set.seed(123)
colnames(underdog.dat.1)
boruta.train <- Boruta(`BIG.UPSET`~., data = underdog.dat.1[,-c(1:15)], doTrace = 2)
print(boruta.train)

boruta.train$finalDecision == "Confirmed"

plot(boruta.train, xlab = "", xaxt = "n")
lz<-lapply(1:ncol(boruta.train$ImpHistory),function(i)
boruta.train$ImpHistory[is.finite(boruta.train$ImpHistory[,i]),i])
names(lz) <- colnames(boruta.train$ImpHistory)
Labels <- sort(sapply(lz,median))
axis(side = 1,las=2,labels = names(Labels),
       at = 1:ncol(boruta.train$ImpHistory), cex.axis = 0.7)

final.boruta <- TentativeRoughFix(boruta.train)
boruta.df <- attStats(final.boruta)
print(final.boruta)

(imp.attrs<-getSelectedAttributes(final.boruta, withTentative = F))

# Dataframe of importance for only confirmed variables
final.imp.dat<-boruta.df[rownames(boruta.df) %in% imp.attrs,]

# Sort 
final.imp.dat<-final.imp.dat[order(final.imp.dat$meanImp, decreasing = T),]
# scale meanImp to sum to 100
final.imp.dat$meanImp<-final.imp.dat$meanImp/sum(final.imp.dat$meanImp)
final.imp.dat$meanImp<-final.imp.dat$meanImp*100
sum(final.imp.dat$meanImp)
final.imp.dat.full<-final.imp.dat

## Run 2
underdog.dat.2<-underdog.dat %>% dplyr::select(-c("BY.YEAR.NO", "BY.ROUND.NO", matches(".RANK")))
underdog.dat.2$UNDERDOG.R.SCORE<-NULL # No current data for this
set.seed(123)
colnames(underdog.dat.2)
boruta.train <- Boruta(`BIG.UPSET`~., data = underdog.dat.2[,-c(1:15)], doTrace = 2)
print(boruta.train)

boruta.train$finalDecision == "Confirmed"

plot(boruta.train, xlab = "", xaxt = "n")
lz<-lapply(1:ncol(boruta.train$ImpHistory),function(i)
  boruta.train$ImpHistory[is.finite(boruta.train$ImpHistory[,i]),i])
names(lz) <- colnames(boruta.train$ImpHistory)
Labels <- sort(sapply(lz,median))
axis(side = 1,las=2,labels = names(Labels),
     at = 1:ncol(boruta.train$ImpHistory), cex.axis = 0.7)

final.boruta <- TentativeRoughFix(boruta.train)
print(final.boruta)

(imp.attrs<-getSelectedAttributes(final.boruta, withTentative = F))

# Dataframe of importance for only confirmed variables
final.imp.dat<-boruta.df[rownames(boruta.df) %in% imp.attrs,]

# Sort 
final.imp.dat<-final.imp.dat[order(final.imp.dat$meanImp, decreasing = T),]
# scale to 100
final.imp.dat$meanImp<-final.imp.dat$meanImp/sum(final.imp.dat$meanImp)
final.imp.dat$meanImp<-final.imp.dat$meanImp*100

sum(final.imp.dat$meanImp)
final.imp.dat.stats<-final.imp.dat

## Export
write.xlsx(final.imp.dat.stats, "Underdog Stat Importance.xlsx", rowNames = T)

## Get 2025 Data
(torvikResume.dat<-cbd_torvik_current_resume())
(torvikMetrics.dat<-cbd_all_metrics(2025))
(torvikTRank<-cbd_torvik_resume_database(2025))
cbd_torvik_current_resume()
(tovikTable.dat<-read.xlsx("./Torvik Full Table.xlsx"))
(kp.dat<-kp_pomeroy_ratings(2025))

names(torvikMetrics.dat)
# team
# conf
# net_rank
names(torvikResume.dat)
# resume
# wab
# elo
names(kp.dat)
# adj_d
# adj_o
# adj_em
names(tovikTable.dat)
# Elite.SOS
# Talent

## Ensure Team names are same
torvik.team.align<-which(!torvikMetrics.dat$team %in% torvikResume.dat$team)
kp.team.align<-which(!kp.dat$team %in% torvikMetrics.dat$team)
kp.dat$team[kp.team.align]

kp.dat$team<-case_when(
  kp.dat$team == "McNeese" ~ "McNeese St.",
  kp.dat$team == "CSUN" ~ "Cal St. Northridge",
  kp.dat$team == "SIUE" ~ "SIU Edwardsville",
  kp.dat$team == "Nicholls" ~ "Nicholls St.",
  kp.dat$team == "Kansas City" ~ "UMKC",
  kp.dat$team == "East Texas A&M" ~ "Texas A&M Commerce" ,
  kp.dat$team == "Southeast Missouri" ~ "Southeast Missouri St.",
  T ~ kp.dat$team
)
(kp.team.align<-which(!kp.dat$team %in% torvikMetrics.dat$team))
(torvik.team.align<-which(!torvikMetrics.dat$team %in% torvikResume.dat$team))

current.dat<-left_join(torvikMetrics.dat %>% dplyr::select(c(team, conf, net_rank)),
                       torvikResume.dat %>% dplyr::select(c(team, resume, wab, elo)), by = "team") %>%
  left_join(kp.dat %>% dplyr::select(c(team, adj_d, adj_o, adj_em)), by = "team") %>%
  left_join(tovikTable.dat %>% dplyr::select(c(Team, Elite.SOS, Talent)) %>% rename(team = Team), by ="team")

current.dat<-current.dat[-1,]
head(current.dat)

# Match names
names(current.dat)<-c("TEAM", "CONFERENCE", "NET.RPI", "RESUME", "WAB", "ELO", "KADJ.D", "KADJ.O", "KADJ.EM", "Elite.SOS", "Talent")
underdog.dat.2.sims<-underdog.dat.2 %>% dplyr::select(c(YEAR,FAVORITE, HIGH.SEED, UNDERDOG,UNDERDOG.SEED, UNDERDOG.CONF, 
                                            SEED.DIFF, BIG.UPSET, matches(paste0("UNDERDOG.",names(current.dat)[-c(1:2)]))))%>%
  dplyr::filter(BIG.UPSET == 1) %>%
  dplyr::select(-c(BIG.UPSET))

# Calculate mean underdog team
underdog.dat.2.sims<-rbind(underdog.dat.2.sims,c(NA,NA,NA, "AVERAGE UNDERDOG", mean(underdog.dat.2.sims$UNDERDOG.SEED),
                                                  NA, mean(underdog.dat.2.sims$SEED.DIFF), colMeans(underdog.dat.2.sims[,-c(1:7)], na.rm = T)))

# FIlter current data to be all teams outside the KenPom top-25
current.dat <- current.dat %>% 
  arrange(-KADJ.EM) %>% tail(-25)

current.dat$YEAR<-rep("2025", nrow(current.dat))
current.dat$SEED.DIFF<-rep(NA, nrow(current.dat))
current.dat$SEED<-rep(NA, nrow(current.dat))
current.dat<-current.dat %>% dplyr::select(c(YEAR, TEAM,  SEED, CONFERENCE, SEED.DIFF, 
                                            everything()))

underdog.dat.2.sims<-underdog.dat.2.sims %>% dplyr::select(-c(FAVORITE, HIGH.SEED)) %>%
  rename(TEAM = UNDERDOG, SEED = UNDERDOG.SEED, CONFERENCE = UNDERDOG.CONF) %>%
  # remove "Underdog." for column names
  rename_all(~sub("UNDERDOG.", "", .))
names(current.dat)[c(1:4, ncol(current.dat)-1, ncol(current.dat))]<-c("YEAR", "TEAM","SEED", "CONFERENCE", "ELITE.SOS", "TALENT")

unique(names(underdog.dat.2.sims) == names(current.dat))

# combine datasets
sims.dat<-rbind(underdog.dat.2.sims, current.dat)

# convert columns 6:ncol to numeric
sims.dat[,5:ncol(sims.dat)]<-sapply(sims.dat[,5:ncol(sims.dat)], as.numeric)
colSums(is.na(sims.dat))

  
# ## KNN
# KNN on full dataset
set.seed(88)
km.out <- kmeans(sims.dat[,-c(1:5)], centers = 10, nstart = 50)
km.out

# Decide how many clusters to look at
n_clusters <- 30

# Initialize total within sum of squares error: wss
wss <- numeric(n_clusters)

set.seed(88)

# Look over 1 to n possible clusters
for (i in 1:n_clusters) {
  # Fit the model: km.out
  km.out <- kmeans(player.norm.dat[,-c(1:5)], centers = i, nstart = 80)
  # Save the within cluster sum of squares
  wss[i] <- km.out$tot.withinss
}

# Produce a scree plot
wss_df <- tibble(clusters = 1:n_clusters, wss = wss)

scree_plot <- ggplot(wss_df, aes(x = clusters, y = wss, group = 1)) +
  geom_point(size = 4)+
  geom_line() +
  scale_x_continuous(breaks = c(2, 4, 6, 8, 10, 12, 14, 16, 18, 20, 22, 24, 26, 28, 30)) +
  xlab('Number of clusters')
scree_plot

scree_plot +
  geom_hline(
    yintercept = wss, 
    linetype = 'dashed', 
    col = c(rep('#000000',19),'#FF0000', rep('#000000',10))
  )
# Going with 27 clusters
set.seed(2)
km.out <- kmeans(player.norm.dat[,-c(1:6)], centers = 12, nstart = 220, iter.max = 50)
km.out

# Calculate euclidean distance between the first point and each other point in the dataset
# Combine column 2 and column 1 of ws.dat to get unique name
nms<-c()
for(i in 1:nrow(player.dat)){
  nms[i]<-paste0(player.dat$Name[i], " - ", player.dat$Position[i])
}
nms

player.dat$Cluster<-km.out$cluster
player.norm.dat$Cluster<-km.out$cluster

(distances <- as.matrix(dist(player.norm.dat[,-c(1:5)], method = 'euclidean', diag = FALSE)))
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
saveWorkbook(wb, file = "./Output/Starting Pitcher Similarity Scores.xlsx", overwrite = TRUE)

