# ## Libraries ##
# library(dplyr)
# library(ggplot2)
# library(e1071)
# library(caret)
# library(XML)
# library(readxl)
# library(tidyr)
# library(tidyverse)
# library(openxlsx)
# library(hoopR)
# library(RCurl)
# library(rvest)
# library(ISLR2)
# library(leaps)
# library(car)
# library(pls)
# library(relaimpo)
# library(forecast)
# library(Boruta)
# library(randomForest)
# library(toRvik)
# library(cbbdata)
source("../../Codes.R")
source("../../Functions.R")
# login(user_email = Sys.getenv("KP_USER"), user_pw = Sys.getenv("KP_PW"))
cbbdata::cbd_login(username = Sys.getenv('CBD_USER'), password = Sys.getenv('CBD_PW'))

## Read in Data
stats.dat <- read.xlsx("./Data/Tournament Matchups.xlsx", sheet = "Tournament Matchups")
names(stats.dat)

# Select data for Underdogs and Favorites by removing columns respective to Favorites/Underdogs or Differences
difference.dat<- stats.dat %>% 
  rename(HIGH.SEED = FAVORITE.SEED, 
         LOW.SEED = UNDERDOG.SEED) %>%
  dplyr::mutate(SCORE.DIFFERENCE = FAVORITE.SCORE - UNDERDOG.SCORE,
                FAVORITE.WIN = ifelse(FAVORITE == WINNER, 1, 0)) %>%
  dplyr::select(-matches(".RANK")) %>%
  dplyr::select("YEAR", "HIGH.SEED", "FAVORITE", "LOW.SEED","FAVORITE.SCORE","UNDERDOG.SCORE", "WINNER", "LOSER", "SCORE.DIFFERENCE","FAVORITE.WIN","SEED.DIFFERENCE", matches("DIFFERENCE."))

## Clean Data 
# Check for NAs
colSums(is.na(difference.dat))
# Outcome as factor
difference.dat$FAVORITE.WIN<-as.factor(difference.dat$FAVORITE.WIN)
# Remove R-SCORE (No Current Data to use for predictions) and `DIFFERENCE.OP.DREB%` as it is a linear combination of OREB%
difference.dat$DIFFERENCE.R.SCORE <- NULL
difference.dat$`DIFFERENCE.OP.DREB%` <- NULL

## Set up for modeling
#Replace %'s in column names
colnames(difference.dat)<-gsub("%", "Pct", colnames(difference.dat))

# Split into Train and Test
set.seed(57)
train.index<-sample(1:nrow(difference.dat), 0.7*nrow(difference.dat))
train.dat<-difference.dat[train.index,]
test.dat<-difference.dat[-train.index,]

# ### ANALYSIS 1 - LOGISTIC REGRESSION - OUTCOME ###
# Run Logistic Regression
set.seed(88)
colnames(train.dat)

logit.model<-glm(FAVORITE.WIN~., data = train.dat[,-c(1:9)], family = binomial) %>%
  stepAIC(trace = TRUE)
summary(logit.model)

# Save Logit model in R environment
save(logit.model, file = "./Data/logit.model - NCAAT Probabilities.RData")

logit.pred<-predict(logit.model, newdata = train.dat, type = "response")
logit.pred.outcome<-ifelse(logit.pred > 0.5, 1, 0) 

table(logit.pred.outcome, train.dat$FAVORITE.WIN)

# Calculate Accuracy, Sensitivity, Specificity
accuracy<-sum(logit.pred.outcome ==train.dat$FAVORITE.WIN)/nrow(train.dat)
sensitivity<-sum(logit.pred.outcome[train.dat$FAVORITE.WIN == 1] == 1)/sum(train.dat$FAVORITE.WIN == 1)
specificity<-sum(logit.pred.outcome[train.dat$FAVORITE.WIN == 0] == 0)/sum(train.dat$FAVORITE.WIN == 0)

## Test Set
logit.pred.test<-predict(logit.model, newdata = test.dat, type = "response")
logit.pred.test.outcome<-ifelse(logit.pred.test > 0.5, 1, 0)
table(logit.pred.test.outcome, test.dat$FAVORITE.WIN)

# Calculate Accuracy, Sensitivity, Specificity
accuracy.test<-sum(logit.pred.test.outcome == test.dat$FAVORITE.WIN)/nrow(test.dat)
sensitivity.test<-sum(logit.pred.test.outcome[test.dat$FAVORITE.WIN == 1] == 1)/sum(test.dat$FAVORITE.WIN == 1)
specificity.test<-sum(logit.pred.test.outcome[test.dat$FAVORITE.WIN == 0] == 0)/sum(test.dat$FAVORITE.WIN == 0)

#### PREDICT POINT TOTALS ####
## Variable Selection for Point Total
colnames(difference.dat)
difference.dat$TOTAL.SCORE<-difference.dat$FAVORITE.SCORE + difference.dat$UNDERDOG.SCORE
ivs<-names(logit.model$coefficients)[-1]
# Run Linear Regression
set.seed(111)
lm.model<-lm(TOTAL.SCORE~., data = difference.dat[,c(ivs, "TOTAL.SCORE")])
summary(lm.model)
# Save Linear Model in R environment
save(lm.model, file = "./Data/lm.model - NCAAT Point Totals.RData")

lm.pred.total<-predict(lm.model, newdata = difference.dat)
 
## Calculate AAE
aae.total<-mean(abs(lm.pred.total - difference.dat$TOTAL.SCORE))

## Get Data of This Year
(torvikResume.dat<-cbd_torvik_current_resume())
(kp.dat<-read.xlsx("./Data/KP Full Table.xlsx"))
(torvikTRank<-cbd_torvik_resume_database(2025))
(torvikTable.dat<-read.xlsx("./Data/Torvik Full Table.xlsx"))
(torvikTeamFactors.dat<-cbbdata::cbd_torvik_team_factors(2025))
 
logit.model$coefficients
names(kp.dat)
# team
# conf
# kp_rating
kp.dat<-kp.dat %>% dplyr::rename(team = TeamName, KADJ.EM=AdjEM)
names(torvikResume.dat)
# team
# resume
# WAB
torvikResume.dat<-torvikResume.dat %>% dplyr::rename(NET=net, ELO=elo, RESUME = resume, WAB=wab, WAB.RANK=wab_rk)
names(torvikTable.dat)
# Team
# Talent
torvikTable.dat<-torvikTable.dat[,-ncol(torvikTable.dat)] %>% dplyr::rename(EFGPct=eFG, TOVPct="TOV%", TALENT=Talent)

# ## Ensure Team names are same
(kp.team.align<-which(!kp.dat$team %in% torvikResume.dat$team))
kp.dat$team[which(!kp.dat$team %in% torvikResume.dat$team)]
sort(torvikResume.dat$team)
kp.dat$team<-case_when(kp.dat$team == "CSUN" ~ "Cal St. Northridge",
                       kp.dat$team == "Kansas City" ~ "UMKC",
                       kp.dat$team == "Nicholls" ~ "Nicholls St.",
                       kp.dat$team == "McNeese" ~ "McNeese St.",
                       kp.dat$team == "SIUE" ~ "SIU Edwardsville" ,
                       kp.dat$team == "Southeast Missouri" ~ "Southeast Missouri St.",
                       kp.dat$team == "East Texas A&M" ~ "Texas A&M Commerce", 
                       T~kp.dat$team)
(kp.team.align<-which(!kp.dat$team %in% torvikResume.dat$team))                   
(kp.team.align<-which(!torvikResume.dat$team %in% kp.dat$team))   
torvikResume.dat$team[which(!torvikResume.dat$team %in% kp.dat$team)]
(torvikTable.team.align<-which(!torvikTable.dat$Team %in% kp.dat$team))
(torvikTable.team.align<-which(!torvikTable.dat$Team %in% torvikResume.dat$team))

# Check for missingness in current data
colSums(is.na(kp.dat))
colSums(is.na(torvikResume.dat))
colSums(is.na(torvikTable.dat))

current.dat<-left_join(kp.dat %>% dplyr::select(c(team, KADJ.EM)),
                       torvikResume.dat %>% dplyr::select(c(team,conf,RESUME, WAB)), by = "team") %>%
  left_join(torvikTable.dat %>% dplyr::select(c(Team, TALENT)), by = c("team" = "Team"))
head(current.dat)
 
current.dat<-current.dat[-1,]
current.dat<-current.dat %>% dplyr::select(c(team, conf, KADJ.EM, WAB, RESUME, TALENT))
current.dat$SEED<-NA

# Create a matrix of all team combinations
team.combos<-expand.grid(current.dat$team, current.dat$team)

names(team.combos)<-c("FAVORITE", "UNDERDOG")

# Join data for FAVORITE from Current.dat
team.combos<-left_join(team.combos, current.dat, by = c("FAVORITE" = "team")) %>%
  rename("FAVORITE Conf" = conf, "FAVORITE KADJ.EM" = KADJ.EM, "FAVORITE WAB" = WAB, "FAVORITE TALENT" = TALENT, "FAVORITE RESUME" = RESUME, "FAVORITE SEED" = `SEED`)
# Join UNDERDOG data
team.combos<-left_join(team.combos, current.dat, by = c("UNDERDOG" = "team")) %>%
  rename("UNDERDOG Conf" = conf, "UNDERDOG KADJ.EM" = KADJ.EM, "UNDERDOG WAB" = WAB, "UNDERDOG TALENT" = TALENT, "UNDERDOG RESUME" = RESUME, "UNDERDOG SEED" = `SEED`)

# Add in Seed
team.combos$`FAVORITE SEED`<-0
team.combos$`UNDERDOG SEED`<-0

# Create difference variables
team.combos$SEED.DIFFERENCE<-team.combos$`FAVORITE SEED` - team.combos$`UNDERDOG SEED`
team.combos$DIFFERENCE.KADJ.EM<-team.combos$`FAVORITE KADJ.EM` - team.combos$`UNDERDOG KADJ.EM`
team.combos$DIFFERENCE.WAB<-team.combos$`FAVORITE WAB` - team.combos$`UNDERDOG WAB`
team.combos$DIFFERENCE.RESUME<-team.combos$`FAVORITE RESUME` - team.combos$`UNDERDOG RESUME`
team.combos$DIFFERENCE.TALENT<-team.combos$`FAVORITE TALENT` - team.combos$`UNDERDOG TALENT`


## Run Logistic Regression on new data
logit.pred.full<-predict(logit.model, newdata = team.combos, type = "response")
logit.pred.outcome.full<-ifelse(logit.pred.full > 0.5, 1, 0)

## Run Linear Regression on new data for Total

# Create a dataframe of the predictions
team.combos$PREDICTION<-logit.pred.outcome.full
team.combos$PREDICTION.PROB<-logit.pred.full*100

# Calculate American Betting Odds from Win Probability
team.combos$BETTING.ODDS<-ifelse(team.combos$PREDICTION == 1, -1*(team.combos$PREDICTION.PROB/(1-(team.combos$PREDICTION.PROB/100))), (100/(team.combos$PREDICTION.PROB/100))-100)

# Predict Point Totals
team.combos$TOTAL.SCORE<-predict(lm.model, newdata = team.combos)

## Predict point spread
#formula <- 1 / (1 + 10^(-x / 400))  # Left-hand side of the equation
prob <- team.combos$PREDICTION.PROB / 100  # Right-hand side of the equation

# Step 2: Express in terms of exponent
# Rearranging: 1 = 0.7 * (1 + 10^(-x/400))
# Expanding: 0.3 = 0.7 * 10^(-x/400)
# Isolating exponent: (3/7) = 10^(-x/400)

# Step 3: Take logarithm base 10 on both sides
log_fraction <- log10((1-prob)/prob)

# Step 4: Solve for x
x <- -400 * log_fraction

# Print result
team.combos$POINT.SPREAD<-abs(round(x, 1)/26.5)*ifelse(x > 0, -1, 1)


final.results<-team.combos %>% dplyr::select(c("FAVORITE", "UNDERDOG", "PREDICTION", "PREDICTION.PROB","POINT.SPREAD", "BETTING.ODDS", "TOTAL.SCORE"))
final.results$PREDICTION<-ifelse(final.results$PREDICTION == 1, final.results$`FAVORITE`, final.results$`UNDERDOG`)
final.results$PREDICTION.PROB<-round(final.results$PREDICTION.PROB, 1)
head(final.results)

wb<-createWorkbook()
addWorksheet(wb, "NCAAT Simulations")
writeData(wb, "NCAAT Simulations", final.results)

