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
library(BSDA)

##### Read in Data ####
games.dat<-read_xlsx("./Data/Game Logs.xlsx", sheet = "Data")
full.season.dat<-data.frame("Team" = unique(games.dat$`Home`), 
                            "EOS Record" = "", check.names = FALSE)

## Get end of Season records for each team in each season and put in a data frame
FinalRecord.dat<-rbind(
  games.dat%>%filter(`Away G` == 161 | `Home G` == 161)%>% filter(`Year`== 2023)%>%
    select(Year, `Home`, `Cum Home Win%`)%>%
    rename(Team = `Home`, `EOS Record` = `Cum Home Win%`),
  games.dat%>%filter(`Away G` == 161 | `Home G` == 161)%>% filter(`Year`== 2023)%>%
    select(Year, `Away`, `Cum Away Win%`)%>%
    rename(Team = `Away`, `EOS Record` = `Cum Away Win%`),
  games.dat%>%filter(`Away G` == 161 | `Home G` == 161)%>% filter(`Year`== 2022)%>%
    select(Year, `Home`, `Cum Home Win%`)%>%
    rename(Team = `Home`, `EOS Record` = `Cum Home Win%`),
  games.dat%>%filter(`Away G` == 161 | `Home G` == 161)%>% filter(`Year`== 2022)%>%
    select(Year, `Away`, `Cum Away Win%`)%>%
    rename(Team = `Away`, `EOS Record` = `Cum Away Win%`),
  games.dat%>%filter(`Away G` == 161 | `Home G` == 161)%>% filter(`Year`== 2021)%>%
    select(Year, `Home`, `Cum Home Win%`)%>%
    rename(Team = `Home`, `EOS Record` = `Cum Home Win%`),
  games.dat%>%filter(`Away G` == 161 | `Home G` == 161)%>% filter(`Year`== 2021)%>%
    select(Year, `Away`, `Cum Away Win%`)%>%
    rename(Team = `Away`, `EOS Record` = `Cum Away Win%`),
  games.dat%>%filter(`Away G` == 161 | `Home G` == 161)%>% filter(`Year`== 2019)%>%
    select(Year, `Home`, `Cum Home Win%`)%>%
    rename(Team = `Home`, `EOS Record` = `Cum Home Win%`),
  games.dat%>%filter(`Away G` == 161 | `Home G` == 161)%>% filter(`Year`== 2019)%>%
    select(Year, `Away`, `Cum Away Win%`)%>%
    rename(Team = `Away`, `EOS Record` = `Cum Away Win%`),
  games.dat%>%filter(`Away G` == 161 | `Home G` == 161)%>% filter(`Year`== 2018)%>%
    select(Year, `Home`, `Cum Home Win%`)%>%
    rename(Team = `Home`, `EOS Record` = `Cum Home Win%`),
  games.dat%>%filter(`Away G` == 161 | `Home G` == 161)%>% filter(`Year`== 2018)%>%
    select(Year, `Away`, `Cum Away Win%`)%>%
    rename(Team = `Away`, `EOS Record` = `Cum Away Win%`)
)
FinalRecord.dat<-data.frame(FinalRecord.dat)

# Check for NA's
unique(is.na(FinalRecord.dat))

# Examine Data
names(FinalRecord.dat)
names(games.dat)

# Put game data into 2 Data Frames to Enable Stacking
games.home<-games.dat[,c(1,5:6,14)]
names(games.home)<-c("Year","Team","G","Cum Win Pct")
games.away<-games.dat[,c(1,3:4,13)]
names(games.away)<-c("Year","Team","G","Cum Win Pct")
unique(is.na(games.home))
unique(is.na(games.away))

# Stack
GamesStacked.dat<-rbind(games.home, games.away)

unique(is.na(GamesStacked.dat))
head(FinalRecord.dat)
head(GamesStacked.dat)

# Join final records with stacked data
all.games.dat<-left_join(GamesStacked.dat, FinalRecord.dat, by = c("Year", "Team"))

# QC
unique(is.na(all.games.dat))
all.games.dat[which(is.na(all.games.dat)),]
head(all.games.dat)
#QC
(25590/30)/161

# Calculate difference
all.games.dat$`Win Pct Diff`<-all.games.dat$`Cum Win Pct`-all.games.dat$`EOS.Record`

# Calculate statistical significance between cumulative and EOS
stat.test<-function(p1, p0, n){
  (p1-p0)/sqrt((p0*(1-p0))/n)
}

all.games.dat$`Stat Test`<-NA
all.games.dat$`P-Value`<-NA
all.games.dat$`Significant Difference`<-NA

for (i in 1:nrow(all.games.dat)) {
  all.games.dat$`Stat Test`[i] <-stat.test(all.games.dat$`Cum Win Pct`[i], all.games.dat$`EOS.Record`[i], all.games.dat$G[i])
  all.games.dat$`P-Value`[i] <- 2 * pnorm(-abs(all.games.dat$`Stat Test`[i]))
  all.games.dat$`Significant Difference`[i]<-ifelse(all.games.dat$`P-Value`[i] < 0.05, 1, 0) #95% CI
}
view(all.games.dat)

# Generate data frame for average win pct difference for each game number
Season.Game.Avgs.dat<-all.games.dat%>%
  group_by(G)%>%
  summarise(`Abs Avg Win Pct Diff` = mean(abs(`Win Pct Diff`)), # Average absolute difference in Win Percentage
            `Avg Win Pct Diff` = mean(`Win Pct Diff`),  # Average Win Percentage Difference
            `Avg Stat Test` = mean(`Stat Test`), #Average z-score at each game number
            `Avg P-Value` = mean(`P-Value`), # Average p-value at each game number
            `Significantly Different Records` = mean(`Significant Difference`), #Percent records statistically different from End of Season record at each Game number
            `Average Expected Win Total Difference` = mean(`Abs Avg Win Pct Diff`)*162) #Average wins different from End of Season wins

# Calculate confidence intervals for Win Pct Difference
Season.Game.Avgs.dat$`CI for Win Total Difference - Upper Bound` <-NULL
Season.Game.Avgs.dat$`CI for Win Total Difference - Lower Bound` <-NULL

for(i in 1:nrow(Season.Game.Avgs.dat)){
  Season.Game.Avgs.dat$`CI for Win Total Difference - Upper Bound`[i]<- prop.test(Season.Game.Avgs.dat$`Abs Avg Win Pct Diff`[i], 
                                                                                 Season.Game.Avgs.dat$`G`[i], alternative = "less", conf.level =0.95)$conf.int[1:2][2]*162
  Season.Game.Avgs.dat$`CI for Win Total Difference - Lower Bound`[i]<- prop.test(Season.Game.Avgs.dat$`Abs Avg Win Pct Diff`[i], 
                                                                                 Season.Game.Avgs.dat$`G`[i]*30, alternative = "less", conf.level =0.95)$conf.int[1:2][1]*162
}


plot(Season.Game.Avgs.dat$G, Season.Game.Avgs.dat$`Avg Win Pct Diff`, type = "l", xlab = "Game Number", ylab = "Average Win Pct Difference", main = "Average Win Pct Difference by Game Number")
plot(Season.Game.Avgs.dat$G, Season.Game.Avgs.dat$`Significantly Different Records`, type = "l", xlab = "Game Number", ylab = "Average Number of Significanly Different Records", main = "Average Win Pct Difference by Game Number")

# # Calculate confidence intervals for Win Total Difference
# prop.test(Season.Game.Avgs.dat$`Win Total Difference`[50], 50, 0)
(all.games.dat<-left_join(all.games.dat, Season.Game.Avgs.dat, by = "G"))
all.games.dat<-all.games.dat%>%
  arrange(G, Year)%>%
  mutate(`Current Wins` = `G`*`Cum Win Pct`,
         `End of Season Wins` = 161*`EOS.Record`,
         `Expected Wins` = `Cum Win Pct`*161,
         `Difference Between Expected and End of Season Wins` = abs(`Expected Wins`-`End of Season Wins`),
         `Win Expectancy Outside CI` = ifelse( `Difference Between Expected and End of Season Wins`>`CI for Win Total Difference - Upper Bound`, 1, 0))%>%
  rename(`End of Season Winning Percentage` = `EOS.Record`,
         `Current Winning Percentage`=`Cum Win Pct`)%>%
  select(Year, Team, G, `Current Wins`, `Current Winning Percentage`,`Expected Wins`, `End of Season Wins`, `Difference Between Expected and End of Season Wins`,
         `End of Season Winning Percentage`, `Win Pct Diff`, `Stat Test`, `P-Value`, `Significant Difference`,
         `Avg Win Pct Diff`, `Abs Avg Win Pct Diff`, `Avg Stat Test`, `Avg P-Value`, `Significantly Different Records`, 
         `Average Expected Win Total Difference`, `CI for Win Total Difference - Upper Bound`, `CI for Win Total Difference - Lower Bound`,
         `Win Expectancy Outside CI` )

summary.dat<-data.frame(
  "Games"=c(1:163),
  "Number of Records Outside CI" = NA,
  "Percent Outside CI"=NA,
  "Average Win Total Difference" = Season.Game.Avgs.dat %>% arrange(G) %>% select(`Average Expected Win Total Difference`),
  "Percent of Records Significantly Different" = Season.Game.Avgs.dat %>% arrange(G) %>% select(`Significantly Different Records`),
  check.names = F)

for(i in 1:nrow(summary.dat)){
  #i<-9
  summary.dat$`Number of Records Outside CI`[i]<-all.games.dat[(all.games.dat$G==i & all.games.dat$`Win Expectancy Outside CI`==1),]%>%nrow()
  summary.dat$`Percent Outside CI`[i]<-all.games.dat[(all.games.dat$G==i & all.games.dat$`Win Expectancy Outside CI`==1),]%>%nrow()/sum(all.games.dat$G==i)
}

RecordConfidence.dat<-list("Full Data" = all.games.dat, 
     "Game Summaries" = summary.dat)

# Write to Excel
write.xlsx(RecordConfidence.dat, "./Data/Records Data by Game.xlsx")
