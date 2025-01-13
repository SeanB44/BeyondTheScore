library(httr)
library(caTools)
library(neuralnet)
library(dplyr)
library(ggplot2)
library(expss)
library(e1071)
library(caret)
library(XML)
library(xml2)
library(rvest)
library(dplyr)
library(readxl)
library(openxlsx)
library(tidyverse)
library(tidyr)
library(BSDA)
library(randomForest)

# ### Read in Data ###
HOF.Pit.dat<-read_xlsx("./Data/WAR per Game.xlsx", sheet = 1)
HOF.Bat.dat<-read_xlsx("./Data/WAR per Game.xlsx", sheet = 2)

names(HOF.Pit.dat)

HOF.Pit.dat<-HOF.Pit.dat%>%
  dplyr::select("Name", "HOF","G", "IP", "WAR", "WAR/G")%>%
  mutate(WAR = as.numeric(WAR), `WAR/G` = as.numeric(`WAR/G`))%>%
  mutate(HOF = ifelse(is.na(HOF), 0, 1))#%>%
  #mutate(HOF = factor(HOF, levels = c(0,1), labels = c("No", "Yes")))
head(HOF.Pit.dat)

HOF.Bat.dat<-HOF.Bat.dat%>%
  dplyr::select("Name", "HOF","G", "PA", "WAR", "WAR/100G", "wRC+", "wRC+/G")%>%
  mutate(WAR = as.numeric(WAR), `WAR/100G` = as.numeric(`WAR/100G`), `wRC+` = as.numeric(`wRC+`), `wRC+/G` = as.numeric(`wRC+/G`))%>%
  mutate(HOF = ifelse(is.na(HOF), 0, 1))#%>%
  #mutate(HOF = factor(HOF, levels = c(0,1), labels = c("No", "Yes")))
head(HOF.Bat.dat)

## Point Biserial Correlation between HOF and WAR/G
library(ltm)
cor.test(HOF.Pit.dat$HOF, HOF.Pit.dat$`WAR/G`)
cor.test(HOF.Pit.dat$HOF, HOF.Pit.dat$`WAR`)
biserial.cor(HOF.Pit.dat$`WAR/G`, HOF.Pit.dat$HOF)
biserial.cor(HOF.Pit.dat$`WAR`, HOF.Pit.dat$HOF)

cor.test(HOF.Bat.dat$HOF, HOF.Bat.dat$`WAR/100G`)
cor.test(HOF.Bat.dat$HOF, HOF.Bat.dat$`WAR`)
biserial.cor(HOF.Bat.dat$`WAR/G`, HOF.Bat.dat$HOF)
biserial.cor(HOF.Bat.dat$`WAR`, HOF.Bat.dat$HOF)


## Split into training and testing data
HOF.Bat.logit.dat<-HOF.Bat.dat
# WAR
HOF.Bat.logit.dat$HOF<-as.factor(HOF.Bat.logit.dat$HOF)
set.seed(101)
split<-sample.split(HOF.Bat.logit.dat$HOF, SplitRatio = 0.75)
train.dat<-subset(HOF.Bat.logit.dat, split == TRUE)
test.dat<-subset(HOF.Bat.logit.dat, split == FALSE)

## Fit a linear regression model - just WP
logit.fit<-glm(`HOF`~`WAR`, family = "binomial",data = train.dat)
summary(logit.fit) #AIC: 308.8
# Save summary
WAR.Bat.model<-summary(logit.fit)

# Make predictions
logit.pred<-predict(logit.fit, newdata = test.dat, type = "response")
predictions<-ifelse(logit.pred > 0.20, 1, 0)
predictions = factor(predictions)

regression_predictions_df = data.frame("Predicted" = predictions, "Observed" = test.dat$HOF)

regression_confusion_matrix = confusionMatrix(regression_predictions_df$Predicted, regression_predictions_df$Observed, positive = "1")
regression_confusion_matrix
regression_confusion_matrix$overall["Accuracy"]
#' Accuracy = 0.868
regression_confusion_matrix$byClass["Sensitivity"]
#' Sensitivity = 0.8571
regression_confusion_matrix$byClass["Specificity"]
#' Specificity = 0.8702

# WAR/G
HOF.Bat.logit.dat$HOF<-as.factor(HOF.Bat.logit.dat$HOF)
set.seed(101)
split<-sample.split(HOF.Bat.logit.dat$HOF, SplitRatio = 0.75)
train.dat<-subset(HOF.Bat.logit.dat, split == TRUE)
test.dat<-subset(HOF.Bat.logit.dat, split == FALSE)

## Fit a linear regression model - just WP
logit.fit<-glm(`HOF`~`WAR/G`, family = "binomial",data = train.dat)
summary(logit.fit) #AIC: 569.1
# Save summary
WARperG.Bat.model<-summary(logit.fit)

# Make predictions
logit.pred<-predict(logit.fit, newdata = test.dat, type = "response")
predictions<-ifelse(logit.pred > 0.18, 1, 0)
predictions = factor(predictions)

regression_predictions_df = data.frame("Predicted" = predictions, "Observed" = test.dat$HOF)

regression_confusion_matrix = confusionMatrix(regression_predictions_df$Predicted, regression_predictions_df$Observed, positive = "1")
regression_confusion_matrix
regression_confusion_matrix$overall["Accuracy"]
#' Accuracy = 0.804
regression_confusion_matrix$byClass["Sensitivity"]
#' Sensitivity = 0.8095
regression_confusion_matrix$byClass["Specificity"]
#' Specificity = 0.8029
#' 
## Pitchers
## Split into training and testing data
HOF.Pit.logit.dat<-HOF.Pit.dat
# WAR
HOF.Pit.logit.dat$HOF<-as.factor(HOF.Pit.logit.dat$HOF)
set.seed(101)
split<-sample.split(HOF.Pit.logit.dat$HOF, SplitRatio = 0.667)
train.dat<-subset(HOF.Pit.logit.dat, split == TRUE)
test.dat<-subset(HOF.Pit.logit.dat, split == FALSE)

## Fit a linear regression model - just WP
logit.fit<-glm(`HOF`~`WAR`, family = "binomial",data = train.dat)
summary(logit.fit) #AIC: 159.5
# Save summary
WAR.Pit.model<-summary(logit.fit)

# Make predictions
logit.pred<-predict(logit.fit, newdata = test.dat, type = "response")
predictions<-ifelse(logit.pred > 0.08, 1, 0)
predictions = factor(predictions)

regression_predictions_df = data.frame("Predicted" = predictions, "Observed" = test.dat$HOF)

regression_confusion_matrix = confusionMatrix(regression_predictions_df$Predicted, regression_predictions_df$Observed, positive = "1")
regression_confusion_matrix
regression_confusion_matrix$overall["Accuracy"]
#' Accuracy = 0.841
regression_confusion_matrix$byClass["Sensitivity"]
#' Sensitivity = 0.7037
regression_confusion_matrix$byClass["Specificity"]
#' Specificity = 0.8585

# WAR/G
HOF.Pit.logit.dat$HOF<-as.factor(HOF.Pit.logit.dat$HOF)
set.seed(101)
split<-sample.split(HOF.Pit.logit.dat$HOF, SplitRatio = 0.667)
train.dat<-subset(HOF.Pit.logit.dat, split == TRUE)
test.dat<-subset(HOF.Pit.logit.dat, split == FALSE)

## Fit a linear regression model - just WP
logit.fit<-glm(`HOF`~`WAR/G`, family = "binomial",data = train.dat)
summary(logit.fit) #AIC: 269.77
# Save summary
WARperG.Pit.model<-summary(logit.fit)

# Make predictions
logit.pred<-predict(logit.fit, newdata = test.dat, type = "response")
predictions<-ifelse(logit.pred > 0.12, 1, 0)
predictions = factor(predictions)

regression_predictions_df = data.frame("Predicted" = predictions, "Observed" = test.dat$HOF)

regression_confusion_matrix = confusionMatrix(regression_predictions_df$Predicted, regression_predictions_df$Observed, positive = "1")
regression_confusion_matrix
regression_confusion_matrix$overall["Accuracy"]
#' Accuracy = 0.804
regression_confusion_matrix$byClass["Sensitivity"]
#' Sensitivity = 0.8095
regression_confusion_matrix$byClass["Specificity"]
#' Specificity = 0.8029
#' 