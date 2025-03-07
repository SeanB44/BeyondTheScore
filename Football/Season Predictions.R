source("../../Football Functions.R")
library(dplyr)

### Read in data ###
team.info<-cfbd_team_info(year = "2024")%>%
  dplyr::select(team_id, school)%>%
  rename(team = school)

recruit.dat<-rbind(cfbd_recruiting_team(year = "2024"),
            cfbd_recruiting_team(year = "2023"),
            cfbd_recruiting_team(year = "2022"),
            cfbd_recruiting_team(year = "2021"),
            cfbd_recruiting_team(year = "2020"),
            cfbd_recruiting_team(year = "2019"),
            cfbd_recruiting_team(year = "2018"),
            cfbd_recruiting_team(year = "2017"),
            cfbd_recruiting_team(year = "2016"),
            cfbd_recruiting_team(year = "2015"))
recruit.dat<-recruit.dat%>%
  dplyr::select(team,year, points)%>%
  rename(recruiting_score = points)%>%
  filter(team %in% team.info$team)

SRS_rating.dat<-rbind(cfbd_ratings_srs(year = "2023"),
            cfbd_ratings_srs(year = "2022"),
            cfbd_ratings_srs(year = "2021"),
            cfbd_ratings_srs(year = "2020"),
            cfbd_ratings_srs(year = "2019"),
            cfbd_ratings_srs(year = "2018"),
            cfbd_ratings_srs(year = "2017"),
            cfbd_ratings_srs(year = "2016"),
            cfbd_ratings_srs(year = "2015"),
            cfbd_ratings_srs(year = "2014"))%>%
  filter(team %in% team.info$team)

SRS_rating.dat<-SRS_rating.dat%>%
  dplyr::select(year,team,rating)%>%
  rename(srs_rating = rating)%>%
  filter(team %in% team.info$team)


elo_rating.dat<-rbind(cfbd_ratings_elo(year = "2023"),
                      cfbd_ratings_elo(year = "2022"),
                      cfbd_ratings_elo(year = "2021"),
                      cfbd_ratings_elo(year = "2020"),
                      cfbd_ratings_elo(year = "2019"),
                      cfbd_ratings_elo(year = "2018"),
                      cfbd_ratings_elo(year = "2017"),
                      cfbd_ratings_elo(year = "2016"),
                      cfbd_ratings_elo(year = "2015"),
                      cfbd_ratings_elo(year = "2014"))%>%
  filter(team %in% team.info$team)

prior_elo_rating.dat<-elo_rating.dat%>%
  mutate(year = year+1)%>%
  rename(prior_elo_rating = elo)%>%
  filter(team %in% team.info$team)

## Transfer Portal
transfer.dat<-read_xlsx("./Transfer Portal Data.xlsx", sheet = "Transfers Stacked")

## Returning Production
return_production.dat<-rbind(cfbd_player_returning(year = "2024"),
            cfbd_player_returning(year = "2023"),
            cfbd_player_returning(year = "2022"),
            cfbd_player_returning(year = "2021"),
            cfbd_player_returning(year = "2020"),
            cfbd_player_returning(year = "2019"),
            cfbd_player_returning(year = "2018"),
            cfbd_player_returning(year = "2017"),
            cfbd_player_returning(year = "2016"),
            cfbd_player_returning(year = "2015"))%>%
  rename(year = season)%>%
  filter(team %in% team.info$team)

return_production.dat<-return_production.dat%>%
  dplyr::select(year, team, total_ppa)%>%
  rename(return_ppa = total_ppa)%>%
  filter(team %in% team.info$team)

talent.dat<-rbind(cfbd_team_talent(year = "2023"),
            cfbd_team_talent(year = "2022"),
            cfbd_team_talent(year = "2021"),
            cfbd_team_talent(year = "2020"),
            cfbd_team_talent(year = "2019"),
            cfbd_team_talent(year = "2018"),
            cfbd_team_talent(year = "2017"),
            cfbd_team_talent(year = "2016"),
            cfbd_team_talent(year = "2015"),
            cfbd_team_talent(year = "2014"))%>%
  rename(team = school)%>%
  rename(returning_talent = talent)%>%
  mutate(year = year+1)%>%
  filter(team %in% team.info$team)

prior_srs_rating<-SRS_rating.dat%>%
  mutate(year = year+1)%>%
  rename(prior_srs_rating = srs_rating)%>%
  filter(team %in% team.info$team)

fpi.dat<-rbind(espn_ratings_fpi(year = "2024"),
                  espn_ratings_fpi(year = "2023"),
                  espn_ratings_fpi(year = "2022"),
                  espn_ratings_fpi(year = "2021"),
                  espn_ratings_fpi(year = "2020"),
                  espn_ratings_fpi(year = "2019"),
                  espn_ratings_fpi(year = "2018"),
                  espn_ratings_fpi(year = "2017"),
                  espn_ratings_fpi(year = "2016"),
                  espn_ratings_fpi(year = "2015"))%>%
  rename(team = team_name) %>%
  mutate(year = year)%>%
  dplyr::select(year, team,team_id, fpi )#%>%
  # filter(team %in% team.info$team)

prior_fpi.dat<-rbind(espn_ratings_fpi(year = "2023"),
               espn_ratings_fpi(year = "2022"),
               espn_ratings_fpi(year = "2021"),
               espn_ratings_fpi(year = "2020"),
               espn_ratings_fpi(year = "2019"),
               espn_ratings_fpi(year = "2018"),
               espn_ratings_fpi(year = "2017"),
               espn_ratings_fpi(year = "2016"),
               espn_ratings_fpi(year = "2015"),
               espn_ratings_fpi(year = "2014"))%>%
  rename(team = team_name, fpi_prior = fpi) %>%
  mutate(year = year+1)%>%
  dplyr::select(year, team,team_id, fpi_prior)#%>%
  #filter(team %in% team.info$team)

## FPI dataframe for regression calculations
fpi.mod.dat<-left_join(fpi.dat, prior_fpi.dat, by = c("team","year", "team_id")) 
fpi.mod.dat<-na.omit(fpi.mod.dat)
fpi.mod.dat$fpi<-as.numeric(fpi.mod.dat$fpi)
fpi.mod.dat$fpi_prior<-as.numeric(fpi.mod.dat$fpi_prior)

## SRS dataframe for regression calculations
srs.mod.dat<-left_join(SRS_rating.dat, prior_srs_rating, by = c("team","year")) 
srs.mod.dat<-na.omit(srs.mod.dat)

## Elo dataframe for regression calculations
elo.mod.dat<-left_join(elo_rating.dat, prior_elo_rating.dat, by = c("team","year")) 
elo.mod.dat<-na.omit(elo.mod.dat)

## Linear Regression to Current FPI from last years FPI
fpi.mod<-lm(fpi ~ fpi_prior, data = fpi.mod.dat)
summary(fpi.mod)
fpi_coef<-coef(fpi.mod)[2]

## Linear Regression to Current SRS from last years EOS SRS
srs.mod<-lm(srs_rating ~ prior_srs_rating, data = srs.mod.dat)
cor(srs.mod.dat$srs_rating, srs.mod.dat$prior_srs_rating)
summary(srs.mod)
srs_coef<-coef(srs.mod)[2]

## Linear Regression to Current Elo from last years Elo
elo.mod<-lm(elo ~ prior_elo_rating, data = elo.mod.dat)
summary(elo.mod)
elo_coef<-coef(elo.mod)[2]
# ## Check
# unique(team.info$team %in% SRS_rating.dat$team)
# unique(transfer.dat$team %in% SRS_rating.dat$team)
# unique(transfer.dat$team %in% return_production.dat$team)
# unique(return_production.dat$team %in% recruit.dat$team)
# unique(return_production.dat$team %in% prior_srs_rating$team)
# unique(prior_fpi.dat$team %in% prior_srs_rating$team)
# unique(prior_fpi.dat$team %in% elo_rating.dat$team)
# unique(prior_elo_rating.dat$team %in% elo_rating.dat$team)

full.dat<-full_join(team.info,recruit.dat, by = "team")%>%
  full_join(SRS_rating.dat, by = c("team","year"))%>%
  full_join(transfer.dat, by = c("team","year"))%>%
  full_join(return_production.dat, by = c("team","year"))%>%
  full_join(talent.dat, by = c("team","year"))%>%
  full_join(prior_srs_rating, by = c("team","year"))%>%
  full_join(prior_fpi.dat, by = c("team_id","year"))%>%
  full_join(elo_rating.dat %>% 
              rename(team.x = team), by = c("team.x","year"))%>%
  full_join(prior_elo_rating.dat %>%
              rename(team.x = team), by = c("team.x","year"))%>%
  dplyr::select(year,team.x,team.y,recruiting_score,srs_rating,portal_points,return_ppa,returning_talent,prior_srs_rating,fpi_prior, elo, prior_elo_rating)


# Remove NA's in Transfer Portal Rating
full.dat$portal_points<-ifelse(is.na(full.dat$portal_points), 0, full.dat$portal_points)

full.dat<-full.dat%>%
  rename(team = team.x)%>%
  dplyr::select(-team.y)

# Make FPI numeric
full.dat$fpi_prior<-as.numeric(full.dat$fpi_prior)

# Make Recruiting Score numeric
full.dat$recruiting_score<-as.numeric(full.dat$recruiting_score)

## Add in FPI Regression
full.dat$fpi_prior<-full.dat$fpi_prior*fpi_coef

## Add in SRS Regression
full.dat$prior_srs_rating<-full.dat$prior_srs_rating*srs_coef

## Add in Elo Regression
full.dat$prior_elo_rating<-full.dat$prior_elo_rating*elo_coef

## Variable for Transfer portal plus Recruiting

sum(is.na(full.dat$portal_points))
sum(is.na(full.dat$recruiting_score))
# Remove NA's in Recruiting score
full.dat$recruiting_score<-ifelse(is.na(full.dat$recruiting_score), 0, full.dat$recruiting_score)
sum(is.na(full.dat$recruiting_score))

# # Calculate z-score for each
# full.dat$recruiting_score_zscore<-calculate_z_score(full.dat$recruiting_score)
# full.dat$transfer_portal_rating_zscore<-calculate_z_score(full.dat$portal_points)
# # Calculate z-score for each
# full.dat$recruiting_score_zscore<-(full.dat$recruiting_score)
# full.dat$transfer_portal_rating_zscore<-(full.dat$portal_points)

# Create new variable
full.dat<-full.dat %>%
  mutate(transfer_recruit = ifelse(year %in% c(2022,2023), portal_points + recruiting_score,recruiting_score*2))
names(full.dat)

## Finalize Dataframe for modeling
str(full.dat)
linear.dat<-full.dat %>%
  dplyr::select("year", "team", "return_ppa", "returning_talent", "prior_srs_rating","fpi_prior","transfer_recruit","prior_elo_rating", "elo", "srs_rating")%>%
  na.omit()

## Run Regression to predict End of Season SRS
str(linear.dat)

## Split into training and testing data
# set.seed(3)
# set.seed(11)
set.seed(29)
train_index<-sample(1:nrow(linear.dat), nrow(linear.dat)*0.7)
train.dat<-linear.dat[train_index,]
test.dat<-linear.dat[-train_index,]

# ## Run Ridge Regression
# library(glmnet)
# set.seed(7871)
# x<-model.matrix(elo ~ #return_ppa + 
#                   returning_talent + 
#                   prior_srs_rating + 
#                   fpi_prior + 
#                   transfer_recruit + 
#                   prior_elo_rating,linear.dat[, 1:(ncol(linear.dat))])
# y<-linear.dat$elo
# 
# # Get indices for train and test sets
# #for(i in 551:600){
# i<-184 #62.9%
# #i<-113 #62.2%
# #i<-584
# set.seed(i)
# 
# subset<-sort(sample(1:nrow(linear.dat),nrow(linear.dat)*.7, replace = F ))
# remain<-which(!(1:nrow(linear.dat))%in%subset)
# train <- subset
# test <- remain
# y.test<-y[test]
# 
# cv.out<-cv.glmnet(x[train,],y[train], alpha=0, nfolds=10)
# plot(cv.out)
# bestlam<-cv.out$lambda.min
# bestlam
# # 12.00
# 
# ridge.mod<-?glmnet(x,y,alpha=0,lambda = bestlam)
# ridge.mod$
# ridge.pred<-predict(ridge.mod,s=bestlam,newx=x[test,])
# mean((ridge.pred-y.test)^2)
# # Mean Absolute Error
# mean(abs(ridge.pred-y.test))
# # MAPE
# mean(abs(ridge.pred-y.test)/y.test)
# 
# #Model cp: MSE=17.1
# #Model bic: MSE=16.0
# #Model adjr2: MSE=17.64
# out<-glmnet(x,y, alpha = 0)
# 
# (coefs<-predict(out,type="coefficients",s=bestlam)[1:19]) #Note, ridge regression does not perform variable selection
# 
# # Mean Absolute Deviance
# mean(abs(ridge.pred-y.test))
# #cp = 3.15
# #BIC = 3.11
# #Adjr2 = 3.33
# # Custom = 3.48


## Linear Regression
linear.model<-lm(elo ~ return_ppa + 
                   #returning_talent + 
                   prior_srs_rating + 
                   fpi_prior + 
                   transfer_recruit + 
                   prior_elo_rating, data = train.dat[,-c(1:2)])
summary(linear.model)

# Test accuracy
test.pred<-predict(linear.model, newdata = test.dat[,-c(1:2)])
test.dat$pred<-test.pred
test.dat$error<-test.dat$elo - test.dat$pred
# Mean Absolute Error
MAE<-mean(abs(test.dat$error))
MAE #132.3
# Mean Absolute Percentage Error
MAPE<-mean(abs(test.dat$error/test.dat$elo))
MAPE #8.6%

## Variable Importance
library(relaimpo)
calc.relimp(linear.model, rela = T)
relativeImportance.out<-data.frame("Variable" = names(calc.relimp(linear.model, rela = T)$lmg), "Relative Importance" = calc.relimp(linear.model, type = "lmg", rela = T)$lmg, check.names = F)
# Sort by decreasing RI
relativeImportance.out<-relativeImportance.out[order(relativeImportance.out$`Relative Importance`, decreasing = T),]
relativeImportance.out

# Test significant difference
StatTest<-boot.relimp(linear.model, B = 1000)
ci<-booteval.relimp(StatTest, norank=T)
ci #Significantly Different

## Export
#write.xlsx(relativeImportance.out, "./Output/Relative Importance for Predicting ELO - 2024.xlsx", rowNames = F)

## Predict ELO for 2024
Predict.2024.dat<-full.dat%>%
  filter(year == 2024)%>%
  dplyr::select("team", "return_ppa", "returning_talent", "prior_srs_rating", "fpi_prior", "transfer_recruit", "prior_elo_rating", "elo")#%>%
  #na.omit()
Predict.2024.dat<-Predict.2024.dat%>%filter(!is.na(fpi_prior))

predicted.2024.elo<-predict.lm(linear.model, newdata = Predict.2024.dat[,-1])
Predict.2024.dat$predicted_elo<-predicted.2024.elo

#### Create Dataframe for Pre-season 2024 Ratings and Rankings ####
# Pull in 2023 End-of-Season ELO and Rankings
elo_2023<-cfbd_ratings_elo(year = "2023")%>%
  dplyr::select("team", "elo")%>%
  rename(`2023 Elo` = elo)

rankings_2023<-cfbd_rankings(year = "2023")%>%
  dplyr::filter(poll == "AP Top 25")%>%
  dplyr::filter(week == max(week))%>%
  dplyr::select("season","school", "rank")%>%
  rename(team = school, year = season, `Final 2023 AP Rank` = rank)
rankings_2023

# Create full 2023 Dataframe
prior_year.dat<-left_join(elo_2023, rankings_2023, by = c("team"))
prior_year.dat<-prior_year.dat%>%
  dplyr::select("team", "2023 Elo", `Final 2023 AP Rank`)%>%
  mutate( `Final 2023 AP Rank` = ifelse(is.na(`Final 2023 AP Rank`), "NR", `Final 2023 AP Rank`))

# Join with 2024 Predictions
PreSeason_2024_full<-left_join(Predict.2024.dat, prior_year.dat, by = c("team"))%>%
  dplyr::select("team", "predicted_elo", `2023 Elo`, "Final 2023 AP Rank")

# Add in conferences to teams
team_conferences<-cfbd_team_info(year = "2024")%>%
  dplyr::select("school", "conference")%>%
  rename(team = school)

PreSeason_2024_full<-left_join(team_conferences,PreSeason_2024_full, by = c("team"))%>%
  arrange(desc(predicted_elo))%>%
  rename(`Predicted 2024 Elo` = predicted_elo)%>%
  rename(Team = team)%>%
  rename(Conference = conference)

AP_Rankings_2024<-cfbd_rankings(year = "2024")%>%
  dplyr::filter(poll == "AP Top 25")%>%
  dplyr::filter(week == min(week))%>%
  dplyr::select("school", "rank")%>%
  rename("Team" = "school", "2024 Pre-Season AP Rank" = "rank")%>%
  dplyr::select(c("Team", "2024 Pre-Season AP Rank"))

(PreSeason_2024_full<-left_join(PreSeason_2024_full, AP_Rankings_2024, by = c("Team")))

# Calculate gain or loss from last year
PreSeason_2024_full<-PreSeason_2024_full%>%
  mutate("2024 vs 2023 Change" = `Predicted 2024 Elo` - `2023 Elo`)%>%
  arrange(desc(`Predicted 2024 Elo`))%>%
  mutate("Percent Change" = `2024 vs 2023 Change`/`2023 Elo`)

## Conference Data
#Find average 2024 Pre-Season ELO for each conference in 
(Conference_Avg_Elo<-PreSeason_2024_full%>%
  group_by(Conference)%>%
  summarise("2024 Average Elo" = mean(`Predicted 2024 Elo`, na.rm = T),
            "2023 Average Elo" = mean(`2023 Elo`, na.rm = T))%>%
  mutate("2024 vs 2023 Change" = `2024 Average Elo` - `2023 Average Elo`)%>%
  arrange(desc(`2024 Average Elo`))%>%
  mutate("Percent Change" = `2024 vs 2023 Change`/`2023 Average Elo`))

##Export all data
wb<-createWorkbook()
addWorksheet(wb, "Pre-Season 2024 Ratings")    
addWorksheet(wb, "Conference Data")
writeData(wb, "Pre-Season 2024 Ratings", PreSeason_2024_full)
writeData(wb, "Conference Data", Conference_Avg_Elo)
#saveWorkbook(wb, "./Output/Pre-Season College Football 2024 Ratings.xlsx", overwrite = TRUE)


## Read in schedule for 2024
schedule.dat<-cfbd_game_info(year = "2024")%>%
  dplyr::select("start_date","week", "home_team", "away_team","venue_id", "home_id" )%>%
  mutate(home_elo = NA)%>%
  mutate(away_elo = NA)

## identify off week(s) for each team
schedule.dat$Post_Bye_Week<-NA
schedule.dat$Post_Bye_Week_Home<-NA
schedule.dat$Post_Bye_Week_Away<-NA
all_teams<-unique(c(schedule.dat$home_team, schedule.dat$away_team))
for(i in all_teams){
  #i<-team.info$team[1]
  schedule.temp<-schedule.dat%>%
    filter(home_team == i | away_team == i)
  off_weeks<-c(1:14)[which(!c(1:14)%in%schedule.temp$week)]
  schedule.temp$Post_Bye_Week[c(which(schedule.temp$week %in% (off_weeks+1)))]<-1
  schedule.temp$Post_Bye_Week<-ifelse(is.na(schedule.temp$Post_Bye_Week),0,schedule.temp$Post_Bye_Week)
  schedule.dat$Post_Bye_Week_Home[which(schedule.dat$home_team == i)]<-schedule.temp$Post_Bye_Week[which(schedule.temp$home_team == i)]
  schedule.dat$Post_Bye_Week_Away[which(schedule.dat$away_team == i)]<-schedule.temp$Post_Bye_Week[which(schedule.temp$away_team == i)]
}


schedule.dat<-left_join(schedule.dat, cfbd_team_info(year = "2024")%>%
  dplyr::select("team_id", "school", "venue_id")%>%
  rename(team = school)%>%
  rename(home_field = venue_id), by = c("home_id" = "team_id"))

schedule.dat$neutral_site<-ifelse(schedule.dat$venue_id == schedule.dat$home_field, 0, 1)

schedule.dat$home_elo<-Predict.2024.dat$predicted_elo[match(schedule.dat$home_team, Predict.2024.dat$team)]
schedule.dat$away_elo<-Predict.2024.dat$predicted_elo[match(schedule.dat$away_team, Predict.2024.dat$team)]

# Add in home and away conference
schedule.dat<-left_join(schedule.dat, cfbd_team_info(year = "2024")%>%dplyr::select("school", "conference"), by = c("home_team" = "school"))%>%
  rename(home_conference = conference)
schedule.dat<-left_join(schedule.dat, cfbd_team_info(year = "2024")%>%dplyr::select("school", "conference"), by = c("away_team" = "school"))%>%
  rename(away_conference = conference)

schedule.dat<-schedule.dat%>%
  dplyr::select("start_date", "week", "home_team", "away_team","home_conference", "away_conference", "Post_Bye_Week_Home","Post_Bye_Week_Away", "neutral_site", "home_elo", "away_elo")


## Monte Carlo Simulation
# 1) Simulate the season 10,000 times
# 2) Use the results to calculate the average number of wins for each team

## Function to simulate game
# Ha = Home win probability
# g = game number

sim_game<-function(Ha, g){
  if(runif(1)<Ha){
    return(c(schedule.dat$home_team[g],
             schedule.dat$Result[g]<-1))
    schedule.dat$Winner[g]<-schedule.dat$home_team[g]
    schedule.dat$Loser[g]<-schedule.dat$away_team[g]
  }else{
    return(c(schedule.dat$away_team[g],
             schedule.dat$Result[g]<-0))
    schedule.dat$Winner[g]<-schedule.dat$away_team[g]
    schedule.dat$Loser[g]<-schedule.dat$home_team[g]
  }
}

schedule.dat$Winner<-""
schedule.dat$Loser<-""
schedule.dat$Result<-NA
schedule.dat$conf_game_winner<-""
schedule.dat$conf_game_loser<-""
schedule.dat$Conf_Result<-NA

schedule.dat$away_elo<-ifelse(is.na(schedule.dat$away_elo), min(min(schedule.dat$home_elo, na.rm = T), min(schedule.dat$away_elo, na.rm=T)), schedule.dat$away_elo)
schedule.dat$home_elo<-ifelse(is.na(schedule.dat$home_elo), min(min(schedule.dat$home_elo, na.rm = T), min(schedule.dat$away_elo, na.rm=T)), schedule.dat$home_elo)

# Initiate data frame to save simulated games
simulated.results<-data.frame("Team" = unique(schedule.dat$home_team))
# Set number of simulations
num.sims<-10000
home_field_adv<-50
K<-30

schedule.dat$home_elo<-ifelse(schedule.dat$neutral_site == 1, 0 + schedule.dat$home_elo, home_field_adv+schedule.dat$home_elo)
schedule.dat$home_elo<-ifelse(schedule.dat$Post_Bye_Week_Home == 1, 25 + schedule.dat$home_elo, 0 + schedule.dat$home_elo)
schedule.dat$away_elo<-ifelse(schedule.dat$Post_Bye_Week_Away == 1, 25 + schedule.dat$away_elo, 0 + schedule.dat$away_elo)

# Identify conference games
schedule.dat$conf_game<-ifelse(schedule.dat$home_conference == schedule.dat$away_conference, 1, 0)
schedule.dat$conf_game<-ifelse(is.na(schedule.dat$conf_game), 0,schedule.dat$conf_game)

# Ha = Home win prob
# Aa = Away win prob
# g = game
for(j in 1:num.sims){
  
  schedule.dat.temp<-schedule.dat
  
  for(g in 1:nrow(schedule.dat.temp)){
    #g<-1
    (Ha<-1/(1+10^(((schedule.dat.temp$away_elo[g]-schedule.dat.temp$home_elo[g]))/400))) #Calc Home win prob from Elo and add Home field advantage
    (Aa<-1/(1+10^((schedule.dat.temp$home_elo[g]-schedule.dat.temp$away_elo[g])/400))) #Calc away win prb from Elo and subtract home field advantage
    
    #Sim game and save result
    (simulated.game<-sim_game(Ha, g))
    (schedule.dat.temp$Result[g]<-as.numeric(simulated.game[2]))
    (schedule.dat.temp$Winner[g]<-simulated.game[1])
    (schedule.dat.temp$Loser[g]<-ifelse(schedule.dat.temp$Result[g]==1, schedule.dat.temp$away_team[g], schedule.dat.temp$home_team[g]))
    
    if(schedule.dat.temp$conf_game[g] != 1){
      schedule.dat.temp$Conf_Result[g]<-NA
      schedule.dat.temp$conf_game_winner[g]<-NA
      schedule.dat.temp$conf_game_loser[g]<-NA
    }else{
    schedule.dat.temp$Conf_Result[g]<-schedule.dat.temp$Result[g]
    schedule.dat.temp$conf_game_winner[g]<-ifelse(schedule.dat.temp$Conf_Result[g]==1, schedule.dat.temp$home_team[g], schedule.dat.temp$away_team[g])
    schedule.dat.temp$conf_game_loser[g]<-ifelse( schedule.dat.temp$Conf_Result[g]==0, schedule.dat.temp$home_team[g], schedule.dat.temp$away_team[g])
    }
    ## Elo Shift from Loser to Winner - (K factor * Forecast Delta)
    #Adjust Elo for home team 
    schedule.dat.temp$home_elo[which(schedule.dat.temp$home_team==schedule.dat.temp$home_team[g])]<-schedule.dat.temp$home_elo[g]+K*(ifelse(schedule.dat.temp$Result[g]==1, 1, 0)-Ha)
    schedule.dat.temp$away_elo[which(schedule.dat.temp$away_team==schedule.dat.temp$home_team[g])]<-schedule.dat.temp$home_elo[g]+K*(ifelse(schedule.dat.temp$Result[g]==1, 1, 0)-Ha)
    
    #Adjust Elo for Away team
    schedule.dat.temp$away_elo[which(schedule.dat.temp$away_team==schedule.dat.temp$away_team[g])]<-schedule.dat.temp$away_elo[g]+K*(ifelse(schedule.dat.temp$Result[g]==1, 0, 1)-Aa)
    schedule.dat.temp$home_elo[which(schedule.dat.temp$home_team==schedule.dat.temp$away_team[g])]<-schedule.dat.temp$away_elo[g]+K*(ifelse(schedule.dat.temp$Result[g]==1, 0, 1)-Aa)
  }
  
  sim.dat<-data.frame(aggregate(Result~Winner, data=schedule.dat.temp, FUN=length))%>%rename("Team" = "Winner")
  sim.dat.conf<-data.frame(aggregate(Conf_Result~conf_game_winner, data=schedule.dat.temp, FUN=length))%>%rename("Team" = "conf_game_winner",  "Conference Result" = "Conf_Result")
  
  names(sim.dat)[2]<-paste0("Wins_",j)
  simulated.results<-left_join(simulated.results, sim.dat, by = "Team")
  names(sim.dat.conf)[2]<-paste0("Conference_Wins_",j)
  simulated.results<-left_join(simulated.results, sim.dat.conf, by = c("Team"))
  print(j)
}

# Replace all NA's in simulated results with 0
simulated.results[is.na(simulated.results)]<-0

## Calculate final Win Totals by Averaging together simulated seasons
#num.sims<-ncol(simulated.results)-1
num.sims 

simulated.results$Projected_Wins<-rowMeans(simulated.results %>% dplyr::select(-matches("Conference_Wins_"), -matches("Team")), na.rm = T)
simulated.results$Projected_Conference_Wins<-rowMeans(simulated.results %>% dplyr::select(matches("Conference_Wins_"), -matches("Team")), na.rm = T)

simulated.results[order(simulated.results$Projected_Wins, decreasing=T),c(1, ncol(simulated.results)-1)]
simulated.results[order(simulated.results$Projected_Conference_Wins, decreasing=T),c(1, ncol(simulated.results))]

simulated.results$Projected_Wins<-round(simulated.results$Projected_Wins,0)
simulated.results$Projected_Conference_Wins<-round(simulated.results$Projected_Conference_Wins,0)

## Calculate Projected Losses
for(i in 1:length(unique(simulated.results$Team))){
  #i<-1
simulated.results$Projected_Losses[i]<-sum(simulated.results$Team[i] == schedule.dat$home_team,simulated.results$Team[i] == schedule.dat$away_team ) - simulated.results$Projected_Wins[i]
simulated.results$Projected_Conference_Losses[i]<-sum((simulated.results$Team[i] == schedule.dat$home_team & schedule.dat$conf_game==1),simulated.results$Team[i] == schedule.dat$away_team & schedule.dat$conf_game==1) - simulated.results$Projected_Conference_Wins[i]
}

## Calculate Win Percentage
simulated.results$Projected_WP<-simulated.results$Projected_Wins/(simulated.results$Projected_Wins + simulated.results$Projected_Losses)
simulated.results$Projected_WP<-round(simulated.results$Projected_WP,3)
simulated.results[order(simulated.results$Projected_WP, decreasing=T),c(1, ncol(simulated.results))]

simulated.results$Projected_Conference_WP<-simulated.results$Projected_Conference_Wins/(simulated.results$Projected_Conference_Wins + simulated.results$Projected_Conference_Losses)

simulations.dat<-simulated.results%>%
  dplyr::select("Team", "Projected_Wins", "Projected_Losses", "Projected_WP", "Projected_Conference_Wins", "Projected_Conference_Losses", "Projected_Conference_WP")%>%
  arrange(desc(Projected_WP))

simulations.dat<-left_join(simulated.results, PreSeason_2024_full, by = "Team")%>%
  dplyr::select("Team", "Conference", "Projected_Wins", "Projected_Losses", "Projected_WP","Projected_Conference_Wins", "Projected_Conference_Losses", "Projected_Conference_WP")%>%
  arrange(Conference, desc(Projected_WP))

simulations.dat<-simulations.dat%>%
  group_by(Conference)%>%
  arrange(desc(Projected_Conference_WP))

## Add in final Elo for each team
Final_Elo.dat<-schedule.dat.temp[,c("week","home_team", "away_team", "home_elo", "away_elo")]%>%
  arrange(desc(home_elo))%>%
  distinct(home_team, home_elo)%>%
  rename("Team" = home_team, "Final Elo" = home_elo)

simulations.dat<-left_join(simulations.dat, Final_Elo.dat, by = "Team")

## Final Data
# Total
final.dat<-simulations.dat%>%
  dplyr::select("Team", "Conference", "Final Elo", "Projected_Wins", "Projected_Losses", "Win %" = "Projected_WP", 
                "Projected_Conference_Wins", "Projected_Conference_Losses", "Conference Win%" = "Projected_Conference_WP")%>%
  mutate("Projected Record" = paste(Projected_Wins, "-", Projected_Losses, sep = ""))%>%
  dplyr::select(-c("Projected_Wins", "Projected_Losses"))%>%
  mutate("Projected Conference Record" = paste(Projected_Conference_Wins, "-", Projected_Conference_Losses, sep = ""))%>%
  dplyr::select(-c("Projected_Conference_Wins", "Projected_Conference_Losses"))%>%
  dplyr::select("Team", "Conference", "Final Elo", "Projected Record", "Win %", "Projected Conference Record", "Conference Win%")%>%
  arrange(desc(`Final Elo`))

# Conference
conference.final.dat<-final.dat %>%
  arrange(Conference, desc(`Conference Win%`))

## Export
wb<-createWorkbook()
addWorksheet(wb, "Final Projected Rankings")
addWorksheet(wb, "Final Conference Rankings")
writeData(wb,"Final Projected Rankings", final.dat)
writeData(wb,"Final Conference Rankings", conference.final.dat)
saveWorkbook(wb, "./Output/Final 2024 Full-Season CFB Projections.xlsx", overwrite = F)

##### Simulate Conference Championship Games
read.xlsx("./Output/Final 2024 Full-Season CFB Projections.xlsx", sheet = "Conference Championship Schedule")

for(j in 1:num.sims){
  
  schedule.dat.temp<-schedule.dat
  
  for(g in 1:nrow(schedule.dat.temp)){
    #g<-1
    (Ha<-1/(1+10^(((schedule.dat.temp$away_elo[g]-schedule.dat.temp$home_elo[g]))/400))) #Calc Home win prob from Elo and add Home field advantage
    (Aa<-1/(1+10^((schedule.dat.temp$home_elo[g]-schedule.dat.temp$away_elo[g])/400))) #Calc away win prb from Elo and subtract home field advantage
    
    #Sim game and save result
    (simulated.game<-sim_game(Ha, g))
    (schedule.dat.temp$Result[g]<-as.numeric(simulated.game[2]))
    (schedule.dat.temp$Winner[g]<-simulated.game[1])
    (schedule.dat.temp$Loser[g]<-ifelse(schedule.dat.temp$Result[g]==1, schedule.dat.temp$away_team[g], schedule.dat.temp$home_team[g]))
    
    if(schedule.dat.temp$conf_game[g] != 1){
      schedule.dat.temp$Conf_Result[g]<-NA
      schedule.dat.temp$conf_game_winner[g]<-NA
      schedule.dat.temp$conf_game_loser[g]<-NA
    }else{
      schedule.dat.temp$Conf_Result[g]<-schedule.dat.temp$Result[g]
      schedule.dat.temp$conf_game_winner[g]<-ifelse(schedule.dat.temp$Conf_Result[g]==1, schedule.dat.temp$home_team[g], schedule.dat.temp$away_team[g])
      schedule.dat.temp$conf_game_loser[g]<-ifelse( schedule.dat.temp$Conf_Result[g]==0, schedule.dat.temp$home_team[g], schedule.dat.temp$away_team[g])
    }
    ## Elo Shift from Loser to Winner - (K factor * Forecast Delta)
    #Adjust Elo for home team 
    schedule.dat.temp$home_elo[which(schedule.dat.temp$home_team==schedule.dat.temp$home_team[g])]<-schedule.dat.temp$home_elo[g]+K*(ifelse(schedule.dat.temp$Result[g]==1, 1, 0)-Ha)
    schedule.dat.temp$away_elo[which(schedule.dat.temp$away_team==schedule.dat.temp$home_team[g])]<-schedule.dat.temp$home_elo[g]+K*(ifelse(schedule.dat.temp$Result[g]==1, 1, 0)-Ha)
    
    #Adjust Elo for Away team
    schedule.dat.temp$away_elo[which(schedule.dat.temp$away_team==schedule.dat.temp$away_team[g])]<-schedule.dat.temp$away_elo[g]+K*(ifelse(schedule.dat.temp$Result[g]==1, 0, 1)-Aa)
    schedule.dat.temp$home_elo[which(schedule.dat.temp$home_team==schedule.dat.temp$away_team[g])]<-schedule.dat.temp$away_elo[g]+K*(ifelse(schedule.dat.temp$Result[g]==1, 0, 1)-Aa)
  }
  
  sim.dat<-data.frame(aggregate(Result~Winner, data=schedule.dat.temp, FUN=length))%>%rename("Team" = "Winner")
  sim.dat.conf<-data.frame(aggregate(Conf_Result~conf_game_winner, data=schedule.dat.temp, FUN=length))%>%rename("Team" = "conf_game_winner",  "Conference Result" = "Conf_Result")
  
  names(sim.dat)[2]<-paste0("Wins_",j)
  simulated.results<-left_join(simulated.results, sim.dat, by = "Team")
  names(sim.dat.conf)[2]<-paste0("Conference_Wins_",j)
  simulated.results<-left_join(simulated.results, sim.dat.conf, by = c("Team"))
  print(j)
}



# #### Create Model to Predict single game point differential based on Elo ####
# 
# games.2023<-cfbd_game_info(year = "2023") %>% dplyr::select(
#   "season", "week", "home_team", "away_team", "home_pregame_elo", "away_pregame_elo", "home_points", "away_points") %>%
#   mutate(elo_diff = home_pregame_elo - away_pregame_elo) %>%
#   mutate(point_diff = home_points - away_points)
# 
# single.game.model<-lm(point_diff ~ elo_diff, data = games.2023)
# summary(single.game.model)
# 
# ## make predictions for 2024
# games.2022<-cfbd_game_info(year = "2022") %>% dplyr::select(
#   "season", "week", "home_team", "away_team", "home_pregame_elo", "away_pregame_elo", "home_points", "away_points") %>%
#   mutate(elo_diff = home_pregame_elo - away_pregame_elo) %>%
#   mutate(point_diff = home_points - away_points)%>%
#   filter(complete.cases(.))
# 

# ## Model for just recruiting and Transfer Portal
# ## Just since 2022 - When 247 began tracking
# recruit_transfer.dat<-full.dat%>%
#   filter(year %in% c(2022,2023))%>%
#   dplyr::select("year", "team", "recruiting_score", "portal_points","returning_talent","return_ppa","prior_elo_rating","elo", "srs_rating")%>%
#   na.omit()
# 
# ## Strictly Transfer Portal vs. Recruiting
# set.seed(787)
# recurit_transfer_linear.model1<-lm(elo ~ recruiting_score + portal_points, data = recruit_transfer.dat[(recruit_transfer.dat$year %in% c(2022, 2023)),-c(1:2)])
# (model1_summary<-summary(recurit_transfer_linear.model1))
# # Summary:
#   # P-Value significant
#   # R^2 = 0.405
#   # Only Recruiting is significant
# 
# calc.relimp(recurit_transfer_linear.model1, rela = T)
# relativeImportanceRecruitTransfer.out1<-data.frame("Variable" = names(calc.relimp(recurit_transfer_linear.model1, type = "lmg", rela = T)$lmg), "Relative Importance" = calc.relimp(recurit_transfer_linear.model1, type = "lmg", rela = T)$lmg, check.names = F)
# # Sort by decreasing RI
# relativeImportanceRecruitTransfer.out1<-relativeImportanceRecruitTransfer.out1[order(relativeImportanceRecruitTransfer.out1$`Relative Importance`, decreasing = T),]
# relativeImportanceRecruitTransfer.out1
# 
# # Test significant difference
# RecruitTransfer_StatTest<-boot.relimp(recurit_transfer_linear.model1, B = 1000)
# ci<-booteval.relimp(RecruitTransfer_StatTest, norank=T)
# ci #Significantly Different
# 
# ## Correlation Matrix
# (corr.matrix<-cor(recruit_transfer.dat[,-c(1:2)]))
# # Summary
#   # SRS + Recruiting = 0.639
#   # SRS + Transfer Portal = 0.393
# 
# ## Model adding in Returning talent & usage
# recurit_transfer_linear.model2<-lm(elo ~ recruiting_score + portal_points + return_ppa, data = recruit_transfer.dat[(recruit_transfer.dat$year %in% c(2021,2022, 2023)),-c(1:2)])
# (model2_summary<-summary(recurit_transfer_linear.model2))
# # Summary:
#   # P-Value significant
#   # R^2 = 0.499
#   # Portal and returning talent not significant
# calc.relimp(recurit_transfer_linear.model2, rela = T)
# relativeImportanceRecruitTransfer.out2<-data.frame("Variable" = names(calc.relimp(recurit_transfer_linear.model2, type = "lmg", rela = T)$lmg), "Relative Importance" = calc.relimp(recurit_transfer_linear.model2, type = "lmg", rela = T)$lmg, check.names = F)
# # Sort by decreasing RI
# relativeImportanceRecruitTransfer.out2<-relativeImportanceRecruitTransfer.out2[order(relativeImportanceRecruitTransfer.out2$`Relative Importance`, decreasing = T),]
# relativeImportanceRecruitTransfer.out2
# 
# ## Recruiting Alone
# recurit_transfer_linear.model3<-lm(elo ~ recruiting_score, data = recruit_transfer.dat[(recruit_transfer.dat$year %in% c(2022, 2023)),-c(1:2)])
# (model3_summary<-summary(recurit_transfer_linear.model3))
# # Summary:
#   # P-Value significant
#   # R^2 = 0.401
#   # Recruiting is significant
# 
# ## Transfer Portal Alone
# recurit_transfer_linear.model4<-lm(elo ~ portal_points, data = recruit_transfer.dat[(recruit_transfer.dat$year %in% c(2022, 2023)),-c(1:2)])
# (model4_summary<-summary(recurit_transfer_linear.model4))
# # Summary:
#   # P-Value significant
#   # R^2 = 0.151
#   # Transfer Portal is significant
# ## Export Results and data
# 
# ## Add in Additional Team Performance Data for 2024
# additional_team_data.dat<-cfbd_rankings(year = "2023")%>%
#   dplyr::filter(poll == "AP Top 25")%>%
#   dplyr::filter(week == max(week))%>%
#   dplyr::select("season","school", "rank", "conference")%>%
#   rename(team = school, year = season)
# 
# additional_team_data.dat<-full_join(additional_team_data.dat, cfbd_game_records(year = "2023")%>%
#   dplyr::select("year","team", "total_wins", "total_losses")%>%
#   rename(Wins = total_wins, Losses = total_losses)%>%
#   mutate(`Winning Percentage` = Wins/(Wins+Losses)), by = c("team","year"))
# 
# full.dat.new<-left_join(full.dat, additional_team_data.dat, by = c("team","year"))
# 
# wb<-createWorkbook()
# addWorksheet(wb, "RI - Recruit+Transfer")
# addWorksheet(wb, "RI - Recruit+Transfer+Return")
# addWorksheet(wb, "Portal Data")
# addWorksheet(wb, "Full Data")
# writeData(wb, "RI - Recruit+Transfer", relativeImportanceRecruitTransfer.out1)
# writeData(wb, "RI - Recruit+Transfer+Return", relativeImportanceRecruitTransfer.out2)
# writeData(wb, "Portal Data", recruit_transfer.dat)
# writeData(wb, "Full Data", full.dat.new)
# ## Export
# saveWorkbook(wb, "./Output/Recruit_Transfer_Portal_Regression.xlsx", overwrite = TRUE)
