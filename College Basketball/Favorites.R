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
#login(user_email = Sys.getenv("KP_USER"), user_pw = Sys.getenv("KP_PW"))
cbbdata::cbd_login(username = Sys.getenv('CBD_USER'), password = Sys.getenv('CBD_PW'))

## Read in Data
upset.dat <- read.xlsx("./Data/Tournament Matchups.xlsx", sheet = "Upsets (4+ Seed Diff)")
stats.dat <- read.xlsx("./Data/Tournament Matchups.xlsx", sheet = "Tournament Matchups")

# ## Filter Data
# stats.dat<-stats.dat %>% filter(SEED.DIFFERENCE >= 4)
# names(stats.dat)

## Select data for Underdogs and Favorites by removing columns respective to Favorites/Underdogs or Differences
favorite.dat<- stats.dat %>% 
  rename(HIGH.SEED = FAVORITE.SEED) %>%
  rename(SEED.DIFF = SEED.DIFFERENCE) %>%
  dplyr::select(-c(matches("UNDERDOG."), matches("DIFFERENCE.")))
head(favorite.dat)

## Filter to get games of 4+ seed difference
favorite.dat<-favorite.dat %>% filter(abs(SEED.DIFF) >= 4)

## Clean Data
# Check for NAs
colSums(is.na(favorite.dat))
# Big Upset as factor
favorite.dat$BIG.UPSET<-as.factor(favorite.dat$BIG.UPSET)
# Remove unneeded columns
colnames(favorite.dat)
favorite.dat.1<-favorite.dat %>% dplyr::select(-c("BY.YEAR.NO", "BY.ROUND.NO"))

## Run 2 - Stats only (No ranks)
favorite.dat.2<-favorite.dat %>% dplyr::select(-c("BY.YEAR.NO", "BY.ROUND.NO", matches(".RANK")))
favorite.dat.2$FAVORITE.R.SCORE<-NULL # No current data for this
set.seed(123)
colnames(favorite.dat.2)

boruta.train <- Boruta(`BIG.UPSET`~., data = favorite.dat.2[,-c(1:15)], doTrace = 2)
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
boruta.df <- attStats(final.boruta)
final.imp.dat<-boruta.df[rownames(boruta.df) %in% imp.attrs,]

# Sort 
final.imp.dat<-final.imp.dat[order(final.imp.dat$meanImp, decreasing = T),]
# scale to 100
final.imp.dat$meanImp<-final.imp.dat$meanImp/sum(final.imp.dat$meanImp)
final.imp.dat$meanImp<-final.imp.dat$meanImp*100

sum(final.imp.dat$meanImp)
final.imp.dat.stats<-final.imp.dat

## Export
write.xlsx(final.imp.dat.stats, "Favorite Stat Importance.xlsx", rowNames = T)

## Get 2025 Data
(torvikResume.dat<-cbd_torvik_current_resume())
(torvikMetrics.dat<-cbd_all_metrics(2025))
(torvikTRank<-cbd_torvik_resume_database(2025))
(torvikTable.dat<-read.xlsx("./Data/Torvik Full Table.xlsx"))
(torvikTeamFactors.dat<-cbbdata::cbd_torvik_team_factors(2025))


names(torvikMetrics.dat)
# team
# conf
# net_rank
# kp_def
# kp_off
# kp_rating
names(torvikResume.dat)
# resume
# wab
# elo
names(torvikTable.dat)
# Talent
names(torvikTeamFactors.dat)
# efg
# oreb_rate
torvikTeamFactors.dat$`OP.DREB%`<-100-torvikTeamFactors.dat$oreb_rate
# OP.DREB% 

# ## Ensure Team names are same
torvik.team.align<-which(!torvikMetrics.dat$team %in% torvikResume.dat$team)
torvikTable.team.align<-which(!torvikTable.dat$team %in% torvikMetrics.dat$team)
torvikTeamFactors.align<-which(!torvikTeamFactors.dat$team %in% torvikMetrics.dat$team)

(torvik.team.align<-which(!torvikMetrics.dat$team %in% torvikResume.dat$team))

current.dat<-left_join(torvikMetrics.dat %>% dplyr::select(c(team, conf, net_rank)),
                       torvikResume.dat %>% dplyr::select(c(team, resume, wab, elo)), by = "team") %>%
  left_join(torvikMetrics.dat %>% dplyr::select(c(team, kp_def, kp_off, kp_rating)), by = "team") %>%
  left_join(torvikTable.dat[,-ncol(torvikTable.dat)] %>% dplyr::select(c(Team, Talent)) %>% rename(team = Team), by ="team") %>%
  left_join(torvikTeamFactors.dat %>% dplyr::select(c(team, efg, oreb_rate, `OP.DREB%`)), by = "team")

current.dat<-current.dat[-1,]
head(current.dat)
 
# Match names
names(current.dat)<-c("TEAM", "CONFERENCE", "NET.RPI", "RESUME", "WAB", "ELO", "KADJ.D", "KADJ.O", "KADJ.EM", "TALENT", "EFG%","OREB%", "OP.DREB%")
favorite.dat.2.sims<-favorite.dat.2 %>% dplyr::select(c(YEAR,FAVORITE, HIGH.SEED, UNDERDOG,WINNER, FAVORITE.CONF,
                                            SEED.DIFF, BIG.UPSET, matches(paste0("FAVORITE.",names(current.dat)[-c(1:2)]))))%>%
  dplyr::filter(SEED.DIFF>=4) %>%
  dplyr::filter(UNDERDOG == WINNER)%>%
  dplyr::filter(BIG.UPSET == 1) %>%
  dplyr::select(-c(BIG.UPSET,`FAVORITE.EFG%D`,WINNER))

# Calculate mean underdog team
favorite.dat.2.sims<-rbind(favorite.dat.2.sims,c(NA,"AVERAGE FAVORITE",mean(favorite.dat.2.sims$HIGH.SEED, na.rm = T),NA,
                                                  NA, mean(favorite.dat.2.sims$SEED.DIFF), colMeans(favorite.dat.2.sims[,-c(1:6)], na.rm = T)))

# # Filter current data to be all teams outside the KenPom top-25
# current.dat <- current.dat %>%
#   arrange(-KADJ.EM) %>% tail(-25)

current.dat$YEAR<-rep("2025", nrow(current.dat))
current.dat$SEED.DIFF<-rep(NA, nrow(current.dat))
current.dat$SEED<-rep(NA, nrow(current.dat))
current.dat<-current.dat %>% dplyr::select(c(YEAR, TEAM,  SEED, CONFERENCE, SEED.DIFF,
                                            everything()))
 
favorite.dat.2.sims<-favorite.dat.2.sims %>% dplyr::select(-c(UNDERDOG)) %>%
  rename(TEAM = FAVORITE, SEED = HIGH.SEED, CONFERENCE = FAVORITE.CONF) %>%
  # remove "Underdog." for column names
  rename_all(~sub("FAVORITE.", "", .))
# names(current.dat)[c(1:3, ncol(current.dat)-1, ncol(current.dat))]<-c("YEAR", "TEAM","SEED", "CONFERENCE", "ELITE.SOS", "TALENT")

unique(names(favorite.dat.2.sims) == names(current.dat))

# combine datasets
sims.dat<-rbind(favorite.dat.2.sims, current.dat)
 
# convert columns 6:ncol to numeric
sims.dat$SEED<-NULL
sims.dat$SEED.DIFF<-NULL
sims.dat$CONFERENCE<-NULL
sims.dat[,3:ncol(sims.dat)]<-sapply(sims.dat[,3:ncol(sims.dat)], as.numeric)
colSums(is.na(sims.dat))

# If Team Name is "Average Underdog" make year "min(year) - max(year)"
sims.dat$YEAR[which(sims.dat$TEAM == "AVERAGE FAVORITE")]<-paste0((min(as.numeric(sims.dat$YEAR))), "-", (max(as.numeric(sims.dat$YEAR))))
colSums(is.na(sims.dat))
 
sims.dat<-sims.dat %>% na.omit()

# Z-Score Normalizaion
apply(sims.dat[,-c(1:2)], 2, function(x) hist(x))
sims.norm.dat<-sims.dat
sims.norm.dat[,-c(1:2)]<-apply(sims.dat[,-c(1:2)], 2, function(x) (x - mean(x))/sd(x))

# ## KNN
# KNN on full dataset
set.seed(8)
km.out <- kmeans(sims.norm.dat[,-c(1:2)], centers = 10, nstart = 50)
km.out

# Decide how many clusters to look at
n_clusters <- 30

# Initialize total within sum of squares error: wss
wss <- numeric(n_clusters)

set.seed(8)

# Look over 1 to n possible clusters
for (i in 1:n_clusters) {
  # Fit the model: km.out
  km.out <- kmeans(sims.norm.dat[,-c(1:2)], centers = i, nstart = 80)
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
# Going with 6 clusters
set.seed(12)
km.out <- kmeans(sims.norm.dat[,-c(1:2)], centers = 6, nstart = 250, iter.max = 50)
km.out

# Calculate euclidean distance between the first point and each other point in the dataset
# Combine column 2 and column 1 of ws.dat to get unique name
nms<-c()
for(i in 1:nrow(sims.dat)){
  nms[i]<-paste0(sims.dat$TEAM[i]," - ", sims.dat$YEAR[i])
}
nms

sims.norm.dat$Cluster<-km.out$cluster
sims.norm.dat$Cluster<-km.out$cluster

(distances <- as.matrix(dist(sims.norm.dat[,-c(1:2)], method = 'euclidean', diag = FALSE)))
# Make 0s into NAs
distances[distances == 0] <- NA
distances.df<-as.data.frame(distances)
colnames(distances.df)<-nms
rownames(distances.df)<-nms
nms[duplicated(nms)]
distances.df

# Sort by Average Underdog
distances.df<-distances.df %>% dplyr::select("AVERAGE FAVORITE - NA-NA", everything())

# sort by first column
distances.df<-distances.df[order(distances.df[,1], decreasing = F),]

# Export distances
wb<-createWorkbook()
addWorksheet(wb, "Distances")
writeData(wb, "Distances", distances.df, rowNames = TRUE, colNames = TRUE)
saveWorkbook(wb, file = "./Output/Favorite Similarity Scores.xlsx", overwrite = TRUE)

