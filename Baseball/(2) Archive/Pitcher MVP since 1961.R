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
player.dat<-read_xlsx("./Data/Position Players With War of 4_9+ since 1947.xlsx", sheet = "All P", skip = 4)
names(player.dat)
player.dat$Rk<-NULL
player.dat<-player.dat%>%
  filter(Season >= 1961)

# Arrange columns to have MVP on end
names(player.dat)
table(player.dat$MVP)

player.dat$Player[player.dat$MVP==1]

# Set IVs
IVs<-names(player.dat)[-c(1:2, 4:5, 38)]
DV<-"MVP"
cor(player.dat$WAR, player.dat$MVP, use = "complete.obs") #0.0.192

## Random Under-sampling
# Separate the majority and minority classes
table(player.dat$MVP)
majority_class <- player.dat %>% dplyr::filter(MVP == 0)
minority_class <- player.dat %>% dplyr::filter(MVP == 1)

# Perform undersampling on the majority class
set.seed(511) # 22 for reproducibility
oversampled_minority_class <- minority_class %>% sample_n(nrow(minority_class)*12, replace = T)
undersampled_majority_class_1 <- majority_class %>% filter(WAR<=8.3) %>% sample_n(nrow(minority_class)*10)
undersampled_majority_class_2 <- majority_class %>% sample_n(nrow(minority_class)*13)


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
  # 1. WAR(0.739)
  # 2. W(0.738)
  # 3. WAA(0.681)
  # 4. W/L%(0.643)
  # 5. WHIP(-0.634)
## Lowest
  # 1. BB(-0.028)
  # 2. H(-0.067)
  # 3. WP(0.110)
  # 4. HR9(-0.130)
  # 5. HBP(-0.153)

sampled.dat$SV<-NULL #Not considering relievers

# Logistic Regression

sampled.dat$MVP<-factor(sampled.dat$MVP)
apply(sampled.dat, 2, function(x) sum(is.na(x))) # Remove NAs

# Fit Initial full model
base.model<-glm(formula = MVP~., data = sampled.dat[-c(1:5)], family = "binomial")
#vif(base.model)
cor(base.model$model[, -1])

base.model<-glm(formula = MVP~., data = sampled.dat[-c(1:5)], family = "binomial")
#vif(base.model)
cor.matrix<-data.frame(cor(base.model$model[, -1]))

# Remove due to multicollinearity
sampled.dat<-sampled.dat%>%select(-c(L, Dec, G, CG, BF, R, BB, IBB))
base.model<-glm(formula = MVP~., data = sampled.dat[-c(1:5)], family = "binomial")
#vif(base.model)
cor.matrix<-data.frame(cor(base.model$model[, -1]))

summary(base.model)
vif(base.model)
cor.matrix<-data.frame(cor(base.model$model[, -1]))

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
trainIndex <- createDataPartition(sampled.dat$MVP, p = .6, 
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
(regression_cutoff = regression_accuraccy@x.values[[1]][c(6)])
# Optimal cutoff for logistic regression model (in terms of accuracy) is 0.553

regression_test_prob <- predict(log.model, test.dat, type = "response")
regression_test_class <- ifelse(regression_test_prob >= regression_cutoff, 1, 0)
regression_test_class <- factor(regression_test_class, labels = c("0","1"))
regression_predictions_df = data.frame("Predicted" = regression_test_class, "Actual" = test.dat$MVP)
regression_confusion_matrix <- confusionMatrix(regression_predictions_df$Predicted, regression_predictions_df$Actual, positive = "1")
regression_confusion_matrix
regression_confusion_matrix$overall["Accuracy"]
#' Accuracy = 1
regression_confusion_matrix$byClass["Sensitivity"]
#' Sensitivity = 1
regression_confusion_matrix$byClass["Specificity"]
#' Specificity = 1

regression_prediction <- prediction(regression_test_prob, test.dat$MVP, label.ordering = c("0","1"))
regression_roc <- performance(regression_prediction, measure = "tpr", x.measure = "fpr")
plot(regression_roc, main = "ROC Curve of Logistic Regression Model", ylab = "Sensitivity", xlab = "1 - Specificity")
abline(a = 0, b = 1)
regression_auc <- performance(regression_prediction, measure = "auc")
regression_auc@y.values[[1]]
# AUC = 1

### Apply Model To Past Data  To Get Probabilities ###
minority_class
historical.preds<-predict(log.model, newdata = minority_class, type = "response")
MVPs.dat<-minority_class%>%
  cbind(historical.preds)%>%
  select(Player, Season, Team, all_of(IVs), historical.preds)%>%
  arrange(desc(historical.preds))

all.preds<-predict(log.model, newdata = player.dat, type = "response")

all.preds.dat<-player.dat%>%
  cbind(all.preds)%>%
  select(Player, Season, Team, all_of(IVs), MVP, all.preds)%>%
  arrange(desc(all.preds))


### Variable Importance ###
sampled.dat<-sampled.dat%>%mutate_if(is.numeric, scale)

# Train/Test Split
set.seed(221)
trainIndex <- createDataPartition(sampled.dat$MVP, p = .6, 
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
# 6
(regression_cutoff = regression_accuraccy@x.values[[1]][c(6)])
# Optimal cutoff for logistic regression model (in terms of accuracy) is 1

regression_test_prob <- predict(log.model.scaled, test.dat, type = "response")
regression_test_class <- ifelse(regression_test_prob >= regression_cutoff, 1, 0)
regression_test_class <- factor(regression_test_class, labels = c("0","1"))
regression_predictions_df = data.frame("Predicted" = regression_test_class, "Actual" = test.dat$MVP)
regression_confusion_matrix <- confusionMatrix(regression_predictions_df$Predicted, regression_predictions_df$Actual, positive = "1")
regression_confusion_matrix
regression_confusion_matrix$overall["Accuracy"]
#' Accuracy = 1
regression_confusion_matrix$byClass["Sensitivity"]
#' Sensitivity = 1
regression_confusion_matrix$byClass["Specificity"]
#' Specificity = 1

regression_prediction <- prediction(regression_test_prob, test.dat$MVP, label.ordering = c("0","1"))
regression_roc <- performance(regression_prediction, measure = "tpr", x.measure = "fpr")
plot(regression_roc, main = "ROC Curve of Logistic Regression Model", ylab = "Sensitivity", xlab = "1 - Specificity")
abline(a = 0, b = 1)
regression_auc <- performance(regression_prediction, measure = "auc")
regression_auc@y.values[[1]]
# AUC = 1

## Calculate relative importance of logistic regression variables
log.model.scaled$coefficients
(coefs <- abs(log.model.scaled$coefficients[-1]))
relaimp.dat <- coefs/sum(coefs)
relaimp.dat 
sort(relaimp.dat, decreasing = T)

## Export
# Coefficients
coef.dat<-data.frame(t(coef(log.model)), check.names = F)
write.csv(coef.dat, "./Pitcher MVP Coefficients.csv", row.names = F)
# Variable importance
pitcher.imp<-data.frame("Relative Importance" = sort(relaimp.dat, decreasing = T), check.names = F)
write.csv(pitcher.imp, "./Pitcher MVP Variable Importance.csv", row.names = T)

