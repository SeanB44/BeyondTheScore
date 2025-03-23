## Libraries ##
# library(httr)
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

source("../../Codes.R")
source("../../Functions.R")
#login(user_email = Sys.getenv("KP_USER"), user_pw = Sys.getenv("KP_PW"))
cbbdata::cbd_login(username = Sys.getenv('CBD_USER'), password = Sys.getenv('CBD_PW'))
### Get Conference Data ###
all_data <- cbbdata::cbd_kenpom_ratings()
all_data <- all_data %>% dplyr::select(year, team,everything())

past.dat<-all_data %>% dplyr::filter(year < 2025) %>% mutate("Tournament" = ifelse(is.na(ncaa_seed) | ncaa_seed == 0, 0, 1)) %>% dplyr::select(-ncaa_seed)
past.dat$Tournament<-as.factor(past.dat$Tournament)

table(past.dat$Tournament)
prop.table(table(past.dat$Tournament))

past.dat$W<-strsplit(past.dat$w_l, "-") %>% sapply("[", 1) %>% as.numeric()
past.dat$L<-strsplit(past.dat$w_l, "-") %>% sapply("[", 2) %>% as.numeric()
past.dat$Wpct<-past.dat$W/(past.dat$W+past.dat$L)
past.dat$w_l<-NULL
past.dat$W<-NULL
past.dat$L<-NULL

## Logistic Regression Model for Predicting Making the Tournament ##
past.dat.model<-past.dat%>%dplyr::select(-matches("_rk"))
set.seed(88)
partition<-createDataPartition(past.dat.model$Tournament, p = 0.8, list = FALSE)
train<-past.dat.model[partition,]
test<-past.dat.model[-partition,]
logit.model<-glm(Tournament ~ rk+adj_em+adj_o+adj_d+adj_t+luck+Wpct, data = train, family = binomial)
summary(logit.model)

## Get Probabilities ##
probabilities<-predict(logit.model, newdata = train, type = "response")
train$Probability<-probabilities
train$Probability<-round(train$Probability, 3)

## Get Predictions ##
train$Prediction<-ifelse(train$Probability > 0.5, 1, 0)
train$Prediction<-as.factor(train$Prediction)

## Get Confusion Matrix ##
confusionMatrix(train$Prediction,train$Tournament)

## Test Data ##
probabilities<-predict(logit.model, newdata = test, type = "response")
test$Probability<-probabilities
test$Probability<-round(test$Probability, 3)

## Get Predictions ##
test$Prediction<-ifelse(test$Probability > 0.5, 1, 0)
test$Prediction<-as.factor(test$Prediction)

## Get Confusion Matrix ##
confusionMatrix(test$Prediction,test$Tournament)


### New Data ###
new.dat<-cbbdata::cbd_all_metrics()[-1,]
new.dat<-new.dat %>% mutate("year" = 2025, "Wpct" = (wins/(wins+losses))) %>% dplyr::select("year","team", "conf", "rk" = "kp_rank", "adj_em" = "kp_rating",
                                                             "adj_o"="kp_off", "adj_d"="kp_def", "adj_t"="kp_tempo","luck"="kp_luck","Wpct")
names(new.dat)                                                             
logit.model$coefficients

## Get Probabilities ##
probabilities<-predict(logit.model, newdata = new.dat, type = "response")
new.dat$Probability<-probabilities
new.dat$Probability<-round(new.dat$Probability, 3)

## Get Predictions ##
new.dat$Prediction<-ifelse(new.dat$Probability > 0.5, 1, 0)
new.dat$Prediction<-as.factor(new.dat$Prediction)

out.dat<-new.dat %>% arrange(desc(Probability)) %>% dplyr::select(team, Probability, Prediction)

write.xlsx(out.dat, "./Output/NCAAT Probability Predictions.xlsx", row.names = FALSE)
