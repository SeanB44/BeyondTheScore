library(httr)
library(caTools)
library(neuralnet)
library(dplyr)
library(ggplot2)
library(expss)
library(e1071)
library(caret)
library(XML)
library(openxlsx)

#unzip('./Data/2024/gl2022.zip', exdir = "./Data/2024")
#unzip('./Data/2024/gl2023.zip', exdir = "./Data/2024")

##### Using last 2 years of games to calculate Home Field Advantage based on Home Win% compared to overall Win% ######
## Read in data
# 2021 Game Log
Games_2021<- read.table("./Data/2024 PRojection Data/gl2021.txt", header=F, sep=",")
glnames<-c("Date", "Away", "Home", "Away_Score", "Home_Score", "DayorNight","Park_ID",
           "Attendence")
head(Games_2021)
# Get only the columns we need
Games2021.dat<-Games_2021[,c(1,4,7,10,11,13,17,18)]
# Set column names
names(Games2021.dat)<-glnames
head(Games2021.dat)

# 2022 Game Log
Games_2022<- read.table("./Data/2024 Projection Data/gl2022.txt", header=F, sep=",")
glnames<-c("Date", "Away", "Home", "Away_Score", "Home_Score", "DayorNight","Park_ID",
           "Attendence")
head(Games_2022)
# Get only the columns we need
Games2022.dat<-Games_2022[,c(1,4,7,10,11,13,17,18)]
# Set column names
names(Games2022.dat)<-glnames
head(Games2022.dat)

# 2023 Game Log
Games_2023<- read.table("./Data/2024 PRojection Data/gl2023.txt", header=F, sep=",")
glnames<-c("Date", "Away", "Home", "Away_Score", "Home_Score", "DayorNight","Park_ID",
           "Attendence")
head(Games_2023)
# Get only the columns we need
Games2023.dat<-Games_2023[,c(1,4,7,10,11,13,17,18)]
# Set column names
names(Games2023.dat)<-glnames
head(Games2023.dat)

# Join to have 2 full years of data (Post-Covid)
Games.full<-rbind(Games2023.dat, Games2022.dat, Games2021.dat)

# Calculate if home team won or lost
Games.full$HomeWin<-ifelse(Games.full$Home_Score>Games.full$Away_Score,1,0)
Games.full$AwayWin<-ifelse(Games.full$Home_Score<Games.full$Away_Score,1,0)
head(Games.full)

## Name Parks ###
sort(unique(Games.full$Park_ID))
sort(unique(Games.full$Home))
# Set team corresponding to park to be ParkID
Games.full$Park_ID<-strtrim(Games.full$Park_ID,3)
#Ensure
xtabs(~Park_ID + Home, data=Games.full)
# Make sure names are equal
Games.full$Park_ID<-case_when(
  Games.full$Park_ID=='PHO' ~ 'ARI',
  Games.full$Park_ID=='ARL' ~ 'TEX',
  Games.full$Park_ID=='CHI' &  Games.full$Home == 'CHA' ~ 'CHA',
  Games.full$Park_ID=='CHI' &  Games.full$Home == 'CHN' ~ 'CHN',
  Games.full$Park_ID=='NYC' &  Games.full$Home == 'NYA' ~ 'NYA',
  Games.full$Park_ID=='NYC' &  Games.full$Home == 'NYN' ~ 'NYN',
  # Special Dyersville
  Games.full$Park_ID=='DYE' ~ 'C',
  T~Games.full$Park_ID
)
#Ensure
xtabs(~Park_ID + Home, data=Games.full)

#Ensure
xtabs(~Park_ID + Home, data=Games.full)

# Make Home team name and Park name same
Games.full$Park_ID<-Games.full$Home
xtabs(~Park_ID + Home, data=Games.full)

## Calculate Home Wins ##
Teams<-unique(Games.full$Home)
# Initiate data frame
records<-data.frame(Team=paste0(Teams),
                    HomeWins=rep(0),
                    HomeLosses=rep(0),
                    HomeWinningPct=rep(0),
                    AwayWins=rep(0),
                    AwayLosses=rep(0),
                    AwayWinningPct=rep(0),
                    TotalWinningPct=rep(0))
for(i in 1:length(Teams)){
  #i<-1
records[i,2]<-sum(Games.full$Home==Teams[i] & Games.full$HomeWin==1)
records[i,3]<-sum(Games.full$Home==Teams[i] & Games.full$AwayWin==1)
records[i,4]<-records[i,2]/sum(records[i,2],records[i,3])
records[i,5]<-sum(Games.full$Away==Teams[i] & Games.full$AwayWin==1)
records[i,6]<-sum(Games.full$Away==Teams[i] & Games.full$HomeWin==1)
records[i,7]<-records[i,5]/sum(records[i,5],records[i,6])
records[i,8]<-sum(records[i,2],records[i,5])/sum(records[i,2], records[i,3], records[i,5], records[i,6])
}
records

### Add in 2024 To Date - 7/22 ###
updated_records<-data.frame("HomeWins" = c(26,25,31,20,26,26,23,24,28,34,26,24,31,27,26,
                          29,17,26,30,22,24,26,37,24,28,22,30,31,27,22),
           "HomeLosses"=c(23,28,19,32,28,22,24,25,21,18,21,26,20,27,23,
                          17,34,24,19,28,24,26,16,25,22,29,22,13,20,26),
           "AwayWins" = c(22,22,28,15,26,26,24,29,24,21,33,15,22,23,22,
                          28,10,25,24,14,25,24,26,26,20,20,30,28,27,23),
           "AwayLosses"=c(30,25,22,32,22,25,29,20,26,27,21,36,28,22,29,
                          25,40,25,25,36,27,22,20,24,30,28,17,26,24,28)
             )
records$HomeWins<-records$HomeWins+updated_records$HomeWins
records$HomeLosses<-records$HomeLosses+updated_records$HomeLosses
records$AwayWins<-records$AwayWins+updated_records$AwayWins
records$AwayLosses<-records$AwayLosses+updated_records$AwayLosses

# Calculate Record Difference
records$HomeSplit<-records$HomeWinningPct-records$TotalWinningPct
records$RelativeDiff<-(records$HomeWinningPct-records$TotalWinningPct)/records$TotalWinningPct
##* Note - Relative diff will be used when calculating win probability for modeling**

#### Investigate ####
histogram(records$HomeSplit)
histogram(records$RelativeDiff) 
qqnorm(records$RelativeDiff) 
qqnorm(records$HomeSplit)

mean(records$HomeSplit)*100 #3.11% greater chance to win vs total
mean(records$RelativeDiff)*100 #6.42% greater chance to win vs Away

#Best Teams
records[which(records$HomeSplit==max(records$HomeSplit)),] #Colorado = +10.4% Home WP vs Total WP
records[which(records$RelativeDiff==max(records$RelativeDiff)),] #Colorado = +25.1% HOme WP vs. Road WP **Colorado is the Outlier**

#Worst
records[which(records$HomeSplit==min(records$HomeSplit)),] #HOUSTON = -0.0021 WP
records[which(records$RelativeDiff==min(records$RelativeDiff)),] #WASHINGTON = -0.52% Chance to win

#T-Tests
stat.test.df<-data.frame(Team=unique(Games.full$Home), `Testing Home vs Away`=rep(0),`Testing vs Rest of League`=rep(0), check.names = F)

for(i in 1:length(stat.test.df$Team)){
  #i<-1
  print(stat.test.df$Team[i])
  print(t.test(Games.full[which(Games.full$Home==stat.test.df$Team[i]),"HomeWin"], Games.full[which(Games.full$Away==stat.test.df$Team[i]),"AwayWin"], alternative = "two.sided", paired = F))
  stat.test.df$`Testing Home vs Away`[i]<-t.test(Games.full[which(Games.full$Home==stat.test.df$Team[i]),"HomeWin"], Games.full[which(Games.full$Away==stat.test.df$Team[i]),"AwayWin"], alternative = "two.sided", paired = F)$p.value
  stat.test.df$`Testing vs Rest of League`[i]<-t.test(Games.full[which(Games.full$Home==stat.test.df$Team[i]),"HomeWin"], Games.full[which(Games.full$Home!=stat.test.df$Team[i]),"AwayWin"], alternative = "two.sided", paired = F)$p.value
}
(stat.test.df$`Significant Difference - Home vs Away`<-ifelse(stat.test.df$`Testing Home vs Away`<0.05, "*", ""))
(stat.test.df$`Significant Difference - vs Rest of League`<-ifelse(stat.test.df$`Testing vs Rest of League`<0.05, "*", ""))
stat.test.df

## Team Names
stat.test.df$`Team Name`<-case_when(
  stat.test.df$Team=='CHA' ~ 'Chicago White Sox',
  stat.test.df$Team=='CHN' ~ 'Chicago Cubs',
  stat.test.df$Team=='NYA' ~ 'New York Yankees',
  stat.test.df$Team=='NYN' ~ 'New York Mets',
  stat.test.df$Team=='CHN' ~ 'Chicago Cubs',
  stat.test.df$Team=='SFN' ~ 'San Francisco Giants',
  stat.test.df$Team=='SLN' ~ 'St. Louis Cardinals',
  stat.test.df$Team=='LAN' ~ 'Los Angeles Dodgers',
  stat.test.df$Team=='BOS' ~ 'Boston Red Sox',
  stat.test.df$Team=='PHI' ~ 'Philadelphia Phillies',
  stat.test.df$Team=='ATL' ~ 'Atlanta Braves',
  stat.test.df$Team=='HOU' ~ 'Houston Astros',
  stat.test.df$Team=='WAS' ~ 'Washington Nationals',
  stat.test.df$Team=='COL' ~ 'Colorado Rockies',
  stat.test.df$Team=='ARI' ~ 'Arizona Diamondbacks',
  stat.test.df$Team=='TEX' ~ 'Texas Rangers',
  stat.test.df$Team=='MIL' ~ 'Milwaukee Brewers',
  stat.test.df$Team=='MIN' ~ 'Minnesota Twins',
  stat.test.df$Team=='DET' ~ 'Detroit Tigers',
  stat.test.df$Team=='CLE' ~ 'Cleveland Guardians',
  stat.test.df$Team=='CIN' ~ 'Cincinnati Reds',
  stat.test.df$Team=='KCA' ~ 'Kansas City Royals',
  stat.test.df$Team=='BAL' ~ 'Baltimore Orioles',
  stat.test.df$Team=='TOR' ~ 'Toronto Blue Jays',
  stat.test.df$Team=='SEA' ~ 'Seattle Mariners',
  stat.test.df$Team=='TBA' ~ 'Tampa Bay Rays',
  stat.test.df$Team=='PIT' ~ 'Pittsburgh Pirates',
  stat.test.df$Team=='OAK' ~ 'Oakland Athletics',
  stat.test.df$Team=='SDN' ~ 'San Diego Padres',
  stat.test.df$Team=='ANA' ~ 'Los Angeles Angels',
  stat.test.df$Team=='MIA' ~ 'Miami Marlins'
)
records.out<-left_join(records,stat.test.df, by="Team")%>%
  select(Team, `Team Name`, HomeWins, HomeLosses, HomeWinningPct, AwayWins, AwayLosses, AwayWinningPct, TotalWinningPct, HomeSplit, RelativeDiff, `Testing Home vs Away`,`Testing vs Rest of League`, `Significant Difference - Home vs Away`,`Significant Difference - vs Rest of League`)%>%
  rename(TeamID = Team, `Team` = `Team Name`, `Home Wins`=HomeWins, `Home Losses`=HomeLosses, `Home Winning Pct`=HomeWinningPct, `Away Wins`=AwayWins, `Away Losses`=AwayLosses, `Away Winning Pct`=AwayWinningPct, `Total Winning Pct`=TotalWinningPct, `Home Split`=HomeSplit, `Relative Difference`=RelativeDiff)

records.out

##Print file
write.xlsx(records.out,paste0('./Output/Home Field Advantage - Raw - ', Sys.Date(), '.xlsx'), rowNames=F)
