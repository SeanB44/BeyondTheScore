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
team.dat<-read_xlsx("./Data/Standings Analysis.xlsx", sheet = "2014-2023", skip = 4)
names(team.dat)

# Make Pythag W/L into Pythag W%
team.dat$PythagWL<-NA
for(i in 1:nrow(team.dat)){
  team.dat$PythagWL[i]<-as.numeric(strsplit(team.dat$pythWL[i], "-")[[1]][1])/(as.numeric(strsplit(team.dat$pythWL[i], "-")[[1]][1])+as.numeric(strsplit(team.dat$pythWL[i], "-")[[1]][2]))
}
team.dat$pythWL<-NULL

# Check Correlations
cor.matrix<-data.frame(cor(as.matrix(team.dat[,c(4:25)])))
cor.matrix.W.L<-cor.matrix%>%
arrange(.,`W.L.`)

# Higest Correlations
  # Rdiff: -.945
  # SRS: .919
  # Total WAR: .912

model.dat<-team.dat%>%
  select(Year, Tm, Rdiff, SOS, SRS, Luck, wOBA, ERA, `WAR-B`, `WAR-P`, DRS, `W-L%`)
names(model.dat)<-c("Year", "Tm", "Rdiff", "SOS", "SRS", "Luck", "wOBA", "ERA", "WAR_B", "WAR_P", "DRS", "W-L%")
# Linear Regression
apply(model.dat, 2, function(x) sum(is.na(x))) # Remove NAs

# Fit Initial full model
base.model<-lm(formula = `W-L%`~., data = model.dat[-c(1:2)])

# Train/Test Split
set.seed(221)
trainIndex <- createDataPartition(model.dat$`W-L%`, p = .7, 
                                  list = FALSE, 
                                  times = 1)
train.dat <- model.dat[ trainIndex,]
test.dat  <- model.dat[-trainIndex,]

mean(train.dat$`W-L%`)
mean(test.dat$`W-L%`)


model<-lm(formula = `W-L%`~., data = train.dat[-c(1:2)])
summary(model)

# Save coefficients
coef_df <- data.frame(coef(model), check.names = F)

## Make Predictions
#model 1
lm_pred <- predict(model, test.dat)

#calculate RMSE
sqrt(mean((lm_pred - test.dat$`W-L%`)^2)) #0073
# Test Accuraccy in wins
test.dat$Predicted_WL_pct<-lm_pred
test.dat$Predicted_Wins<-test.dat$Predicted_WL_pct*162
test.dat$Predicted_Wins<-round(test.dat$Predicted_Wins, 0)
test.dat$Actual_Wins<-test.dat$`W-L%`*162
test.dat$Difference<-test.dat$Predicted_Wins-test.dat$Actual_Wins
mean(abs(test.dat$Difference)) #0.9 wins off

#Model Stats
summary(model)

new.data<-data.frame(Year = 2024, Tm = "CHW", Rdiff = -2.2, SOS = 0.4, SRS = -1.8,
                     Luck = -1, wOBA = 0.277, ERA = 4.87, WAR_B = -5.2, WAR_P = 8.4,
                     DRS = -114.1)

round(predict(model, new.data) * 162)
# 45 Wins
162-45
#45-117 Record
