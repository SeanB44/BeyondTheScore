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
library(survival)

source("../../Codes.R")
source("../../Functions.R")
source("./Update CBB Master.R")
login(user_email = Sys.getenv("KP_USER"), user_pw = Sys.getenv("KP_PW"))

all.dat<-read.xlsx("./Conference Power Rankings Data.xlsx", sheet = "All Data") %>% dplyr::select(c("year", "team", "conf", "ncaa_seed",
                                                                                                    "NC", "RU", "FF", "E8"))
names(all.dat)
colSums(is.na(all.dat))
all.dat$RU[is.na(all.dat$RU)] <- 0

kp.fourfactor.dat<-kp_fourfactors(min_year = 2001, max_year = 2025) %>%
  dplyr::select(year, team, conf, everything())
names(kp.fourfactor.dat)

tourney.dat<-all.dat %>% 
  left_join(kp.fourfactor.dat %>% 
              dplyr::select("year", "team", "conf","adj_t_rk", "adj_o_rk",
                            "adj_d_rk", "off_e_fg_pct_rk","off_to_pct_rk","off_or_pct_rk",
                            "off_ft_rate_rk","def_e_fg_pct_rk", "def_to_pct_rk", "def_or_pct_rk", "def_ft_rate_rk"), by = c("year", "team", "conf"))

names(tourney.dat)
colSums(is.na(tourney.dat))

new.dat<-read.xlsx("./2024-25 CBB Master.xlsx", sheet = "NCAAT", cols = 1:5)
ids.dat<-read.xlsx("./2024-25 CBB Master.xlsx", sheet = "Team IDs", cols = 1:2)

names(ids.dat)<-c("ID", "team")
names(new.dat)<-c("ncaa_seed", "team", "Conference", "AQ", "Rank")

new.dat$team <-ifelse(new.dat$team == "Texas A&amp;M" , "Texas A&M", new.dat$team)

new.dat<-left_join(new.dat, ids.dat, by = c("team")) %>%
  dplyr::select("ID", "team", "ncaa_seed","Conference", "AQ", "Rank") 
head(new.dat)

table(new.dat$ncaa_seed)

tourney.dat<-right_join(ids.dat, tourney.dat, by = c("team"))
colSums(is.na(tourney.dat))

new.dat<-right_join(tourney.dat[tourney.dat$year==2025,] %>% dplyr::select(-ncaa_seed), new.dat %>% dplyr::select(-team), by = c("ID")) 
table(new.dat$ncaa_seed)

# recode NA to 0 in ncaa seed and blanks in Conference, AQ and Rank
new.dat$ncaa_seed[is.na(new.dat$ncaa_seed)] <- 0
new.dat$Conference[is.na(new.dat$Conference)] <- ""
new.dat$AQ[is.na(new.dat$AQ)] <- ""
new.dat$Rank[is.na(new.dat$Rank)] <- ""

tourney.dat<-tourney.dat[tourney.dat$ncaa_seed>0,]
tourney.dat<-tourney.dat[tourney.dat$year!=2020,]
tourney.dat<-tourney.dat[tourney.dat$year>2002,]
tourney.dat<-tourney.dat[tourney.dat$year<2025,]

tourney.dat$End<-rowSums(tourney.dat[, c("NC", "RU", "FF", "E8")])

# Arrange Data

tourney.dat<-tourney.dat %>% dplyr::select( "year","team", "conf",matches("_rk"), "ncaa_seed",
                                            "NC","RU","FF","E8", "End" )

## Linear Regression to predict How far a team will go
set.seed(808)
#train<-sample(1:nrow(tourney.dat), nrow(tourney.dat)*1)
train.dat<-tourney.dat[tourney.dat$year<2025,]
test.dat<-new.dat[new.dat$ncaa_seed!=0,]
# QC
table(test.dat$ncaa_seed)
table(new.dat$ncaa_seed)

## Step-wise Linear regression to predict End
lm1<-lm(End ~ ., data = train.dat %>% dplyr::select(-c("team", "year", "conf", "E8", "NC", "RU", "FF")), na.action = na.omit) %>% stepAIC(trace = TRUE)
summary(lm1)
# predict on new data
lm1.pred<-predict(lm1, newdata = test.dat)
# Ensuring no values below 0
lm1.pred<-ifelse(lm1.pred<0, 0, lm1.pred)
lm1.pred<-ifelse(lm1.pred>4, 4, lm1.pred)
# Scale to a range of 0 to 4
lm1.pred<-lm1.pred*(4/max(lm1.pred))
sum(lm1.pred)
test.dat$prediction<-lm1.pred

#Scale to sum to 8
test.dat$Elite_8<-test.dat$prediction*(8/sum(test.dat$prediction))
# Scale to sum to 4
test.dat$Final_Four<-test.dat$prediction*(4/sum(test.dat$prediction))
# Scale to sum to 2
test.dat$Runner_Up<-test.dat$prediction*(2/sum(test.dat$prediction))
# Scale to sum to 1
test.dat$National_Champion<-test.dat$prediction*(1/sum(test.dat$prediction))

colSums(test.dat[, c("Elite_8", "Final_Four", "Runner_Up", "National_Champion")])

write.xlsx(test.dat, paste0("2025 Tournament Predictions - ", Sys.Date(),".xlsx"), overwrite = TRUE)

