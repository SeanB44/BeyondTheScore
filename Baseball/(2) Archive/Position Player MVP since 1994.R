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

# ### Read in Data ###
player.dat<-read_xlsx("./Data/Position Players With War of 4_9+ since 1947.xlsx", sheet = "All B", skip = 4)
names(player.dat)
player.dat$Rk<-NULL
player.dat<-player.dat%>%
  filter(Season >= 1994)

# Arrange columns to have MVP on end
names(player.dat)
table(player.dat$MVP)

player.dat<-player.dat[,c(1:40, 42:46, 41)]

# Set IVs
IVs<-names(player.dat)[-c(1:2, 4:5, 46)]
DV<-"MVP"
#cor(player.dat$WAR, player.dat$MVP, use = "complete.obs") #0.51

## Random Under-sampling
# Separate the majority and minority classes
table(player.dat$MVP)
majority_class <- player.dat %>% dplyr::filter(MVP == 0)
minority_class <- player.dat %>% dplyr::filter(MVP == 1)

# Perform undersampling on the majority class
set.seed(22) # 22 for reproducibility
oversampled_minority_class <- minority_class %>% sample_n(nrow(minority_class)*3.25, replace = T)
undersampled_majority_class_1 <- majority_class %>% filter(WAR<=6.1) %>% sample_n(nrow(minority_class)*7)
undersampled_majority_class_2 <- majority_class %>% sample_n(nrow(minority_class)*4)


# Combine the undersampled majority class with the minority class
balanced_data <- bind_rows(undersampled_majority_class_1,undersampled_majority_class_2,oversampled_minority_class)

# Check the class distribution of the balanced dataset
table(balanced_data$MVP)
mean(balanced_data$WAR)
mean(player.dat$WAR, na.rm = TRUE)
sd(balanced_data$WAR)
sd(player.dat$WAR, na.rm = TRUE)

sampled.dat<-balanced_data
## Correlations after balancing ##
sort(abs(apply(sampled.dat[,-c(1:5, ncol(sampled.dat))], 2, function(x) cor(x, sampled.dat$MVP))), decreasing = T)
sort(apply(sampled.dat[,-c(1:5, ncol(sampled.dat))], 2, function(x) cor(x, sampled.dat$MVP)), decreasing = T)

## Highest
  # 1. WAR(0.636)
  # 2. WAA(0.593)
  # 3. WARper162(0.558)
  # 4. OPS+(0.548)
  # 5. Rbat(0.544)
## Lowest
  # 1. AB(0.001)
  # 2. SF(0.014)
  # 3. HBP(0.020)
  # 4. TM_WAR_5plus(-0.022)
  # 5. Triples(-0.027)

cor(sampled.dat$TM_WAR_5plus, sampled.dat$MVP) 

# Logistic Regression

sampled.dat$MVP<-factor(sampled.dat$MVP)
apply(sampled.dat, 2, function(x) sum(is.na(x))) # Remove NAs

sampled.dat<-sampled.dat%>%
  select(-c(Rfield))

apply(sampled.dat, 2, function(x) sum(is.na(x)))

# Fit Initial full model
base.model<-glm(formula = MVP~., data = sampled.dat[-c(1:5)], family = "binomial")
#vif(base.model)
cor(base.model$model[, -1])

# Remove AB, OPS and G as they are duplicated (or semi-duplicated) info
sampled.dat<-sampled.dat%>%select(-c(AB, OPS, G))
base.model<-glm(formula = MVP~., data = sampled.dat[-c(1:5)], family = "binomial")
#vif(base.model)
cor.matrix<-data.frame(cor(base.model$model[, -1]))

# Remove WAA, oWAR, Singles, OBP, WARper100G Rbat and SLG% due to correlations
sampled.dat<-sampled.dat%>%select(-c(SLG,OBP))
base.model<-glm(formula = MVP~., data = sampled.dat[-c(1:5)], family = "binomial")
#vif(base.model)
cor.matrix<-data.frame(cor(base.model$model[, -1]))

summary(base.model)

# Remove TB as it is gathered by other more valuble hitting stats
base.model<-glm(formula = MVP~., data = sampled.dat[-c(1:5)], family = "binomial")
summary(base.model)

#vif(base.model)
cor.matrix<-data.frame(cor(base.model$model[, -1]))

# Check which Team WAR
apply(sampled.dat[, c((ncol(sampled.dat)-4):ncol(sampled.dat)-1)], 2, function(x) table(x))
sampled.dat$TM_WAR_9plus<-sampled.dat$TM_WAR_8plus<-sampled.dat$TM_WAR_7plus<-sampled.dat$TM_WAR_6plus<-NULL

# Standardize all variables
#sampled.dat<-sampled.dat%>%mutate_if(is.numeric, scale)

stepmodel <- step(glm(base.model, data = sampled.dat[,-c(1:5)], family = "binomial"), direction = "both", k = 2, trace = 0)
names(stepmodel$coefficients)
names(sampled.dat[, -c(1:5)])[!names(sampled.dat[, -c(1:5)]) %in%names(stepmodel$coefficients)]
vif(stepmodel)

IVs<-names(stepmodel$coefficients)[-1]
DV<-"MVP"
sampled.dat<-sampled.dat%>%select(c(Player, Season, Team, all_of(IVs), DV))

# Train/Test Split
set.seed(221)
trainIndex <- createDataPartition(sampled.dat$MVP, p = .667, 
                                  list = FALSE, 
                                  times = 1)
train.dat <- sampled.dat[ trainIndex,]
test.dat  <- sampled.dat[-trainIndex,]

# Logistic Regression
table(train.dat$MVP)
table(test.dat$MVP)

log.model<-glm(formula = MVP~., data = train.dat[-c(1:3)], family = "binomial")
summary(log.model)

vif(log.model)
# Save coefficients
coef_df <- data.frame(coef(log.model), check.names = F)

## Make Predictions

regression_predicted_prob <- predict(log.model, type = "response")
regression_pred <- prediction(regression_predicted_prob, train.dat$MVP, label.ordering = c("0", "1"))
regression_accuraccy <- performance(regression_pred, measure = "acc")
plot(regression_accuraccy, main = "Accuracy Curve of Logistic Regression Model")
which(regression_accuraccy@y.values[[1]] == max(regression_accuraccy@y.values[[1]]))
# 94 or 92
(regression_cutoff = regression_accuraccy@x.values[[1]][c(58)])
# Optimal cutoff for logistic regression model (in terms of accuracy) is 0.553

regression_test_prob <- predict(log.model, test.dat, type = "response")
regression_test_class <- ifelse(regression_test_prob >= regression_cutoff, 1, 0)
regression_test_class <- factor(regression_test_class, labels = c("0","1"))
regression_predictions_df = data.frame("Predicted" = regression_test_class, "Actual" = test.dat$MVP)
regression_confusion_matrix <- confusionMatrix(regression_predictions_df$Predicted, regression_predictions_df$Actual, positive = "1")
regression_confusion_matrix
regression_confusion_matrix$overall["Accuracy"]
#' Accuracy = .952
regression_confusion_matrix$byClass["Sensitivity"]
#' Sensitivity = .923
regression_confusion_matrix$byClass["Specificity"]
#' Specificity = 0.961

regression_prediction <- prediction(regression_test_prob, test.dat$MVP, label.ordering = c("0","1"))
regression_roc <- performance(regression_prediction, measure = "tpr", x.measure = "fpr")
plot(regression_roc, main = "ROC Curve of Logistic Regression Model", ylab = "Sensitivity", xlab = "1 - Specificity")
abline(a = 0, b = 1)
regression_auc <- performance(regression_prediction, measure = "auc")
regression_auc@y.values[[1]]
# AUC = 0.975

### Apply Model To Past Data  To Get Probabilities ###
minority_class
historical.preds<-predict(log.model, newdata = minority_class, type = "response")

MVPs.dat<-minority_class%>%
  cbind(historical.preds)%>%
  select(Player, Season, Team, all_of(IVs), MVP, historical.preds)%>%
  arrange(desc(historical.preds))

all.preds<-predict(log.model, newdata = player.dat, type = "response")
#all.preds
all.preds.dat<-player.dat%>%
  cbind(all.preds)%>%
  select(Player, Season, Team, Lg, BA, HR, RBI, OPSplus, WAR, MVP, all.preds)%>%
  arrange(desc(all.preds))


### Variable Importance ###
sampled.dat<-sampled.dat%>%mutate_if(is.numeric, scale)

# Train/Test Split
set.seed(221)
trainIndex <- createDataPartition(sampled.dat$MVP, p = .667, 
                                  list = FALSE, 
                                  times = 1)
train.dat <- sampled.dat[ trainIndex,]
test.dat  <- sampled.dat[-trainIndex,]

# Logistic Regression
table(train.dat$MVP)
table(test.dat$MVP)

log.model.scaled<-glm(formula = MVP~., data = train.dat[-c(1:3)], family = "binomial")
summary(log.model.scaled)

vif(log.model.scaled)
# Save coefficients
coef_df <- data.frame(coef(log.model.scaled), check.names = F)

## Make Predictions

regression_predicted_prob <- predict(log.model.scaled, type = "response")
regression_pred <- prediction(regression_predicted_prob, train.dat$MVP, label.ordering = c("0", "1"))
regression_accuraccy <- performance(regression_pred, measure = "acc")
plot(regression_accuraccy, main = "Accuracy Curve of Logistic Regression Model")
which(regression_accuraccy@y.values[[1]] == max(regression_accuraccy@y.values[[1]]))
# 94 or 92
(regression_cutoff = regression_accuraccy@x.values[[1]][c(58)])
# Optimal cutoff for logistic regression model (in terms of accuracy) is 0.415

regression_test_prob <- predict(log.model.scaled, test.dat, type = "response")
regression_test_class <- ifelse(regression_test_prob >= regression_cutoff, 1, 0)
regression_test_class <- factor(regression_test_class, labels = c("0","1"))
regression_predictions_df = data.frame("Predicted" = regression_test_class, "Actual" = test.dat$MVP)
regression_confusion_matrix <- confusionMatrix(regression_predictions_df$Predicted, regression_predictions_df$Actual, positive = "1")
regression_confusion_matrix
regression_confusion_matrix$overall["Accuracy"]
#' Accuracy = .952
regression_confusion_matrix$byClass["Sensitivity"]
#' Sensitivity = .923
regression_confusion_matrix$byClass["Specificity"]
#' Specificity = 0.961

regression_prediction <- prediction(regression_test_prob, test.dat$MVP, label.ordering = c("0","1"))
regression_roc <- performance(regression_prediction, measure = "tpr", x.measure = "fpr")
plot(regression_roc, main = "ROC Curve of Logistic Regression Model", ylab = "Sensitivity", xlab = "1 - Specificity")
abline(a = 0, b = 1)
regression_auc <- performance(regression_prediction, measure = "auc")
regression_auc@y.values[[1]]
# AUC = 0.975

## Calculate relative importance of logistic regression variables
log.model.scaled$coefficients
(coefs <- abs(log.model.scaled$coefficients[-1]))
relaimp.dat <- coefs/sum(coefs)
relaimp.dat 
sort(relaimp.dat, decreasing = T)

## Export
# Coefficients
coef.dat<-data.frame(t(coef(log.model)), check.names = F)
write.csv(coef.dat, "./Position Player MVP Coefficients.csv", row.names = F)
# Variable importance
position.player.imp<-data.frame("Relative Importance" = sort(relaimp.dat, decreasing = T), check.names = F)
write.csv(position.player.imp, "./Position Player MVP Variable Importance.csv", row.names = T)
