library(httr)
#library(ElemStatLearn)
library(caTools)
library(neuralnet)
library(dplyr)
library(expss)
library(e1071)
library(caret)
library(XML)
library(readxl)
library(tidyr)
#install.packages("tidyverse")
library(tidyverse)
# if(!requireNamespace('pacman', quietly = TRUE)){
#   install.packages('pacman')
# }
# pacman::p_load_current_gh("billpetti/baseballr")
library(baseballr)

############# Predicting Hall of Fame Inductees - Likelihood of Induction ################
### Import HoF data
# Set the directory where your Excel files are located
excel_files_dir <- "./HOF/"

## Traditional Ballot Files
# List all Excel files in the directory
excel_files <- list.files(path = excel_files_dir, pattern = "\\_voting.xlsx$", full.names = TRUE)

# Initialize an empty data frame to store combined data
combined_data <- data.frame()

# Loop through each Excel file and combine the data
for (file in excel_files) {
  # Read Excel file into a data frame
  current_data <- read_excel(file, col_names=T, skip=1)
  
  # Combine current data with existing data using rbind
  combined_data <- rbind(combined_data, current_data)
}

# # Print combined data
# print(combined_data)
# names(combined_data)

## Other Committe Files
# List all Excel files in the directory
excel_files <- list.files(path = excel_files_dir, pattern = "\\_OtherCommittee.xlsx$", full.names = TRUE)

# Initialize an empty data frame to store combined data
combined_data_2 <- data.frame()

# Loop through each Excel file and combine the data
for (file in excel_files) {
  # Read Excel file into a data frame
  current_data_2 <- read_excel(file, col_names=T, skip=1)
  
  # Combine current data with existing data using rbind
  combined_data_2 <- rbind(combined_data_2, current_data_2)
}

# # Print combined data
# print(combined_data_2)
# names(combined_data_2)
# names(combined_data)

## Manipulate Dataframes and enable join
combined_data[,ncol(combined_data)]<-NULL #Remove Pos Summary

combined_data<-combined_data%>%
  mutate(HOF = ifelse(`%vote`>0.7500, 1, 0))%>%
  select(-c(YoB, Votes, `%vote`))%>%
  rename(G_Hit = G...13, H_Hit = H...16, HR_Hit = HR...17, BB_Hit = BB...20, 
         G_Pit = G...31, H_Pit = H...35, HR_Pit = HR...36, BB_Pit = BB...37)

combined_data_2<-combined_data_2[,-c(37:40)]%>%
  mutate(HOF = 1)%>%
  select(-`Inducted As`)%>%
  rename(G_Hit = G...11, H_Hit = H...14, HR_Hit = HR...15, BB_Hit = BB...18, 
         W = W...24, L = L...25, G_Pit = G...29, H_Pit = H...33, HR_Pit = HR...34, BB_Pit = BB...35)

# Ensure colnames are the same and in order
all(names(combined_data) == names(combined_data_2))

# Join
full.HOF.dat<-rbind(combined_data, combined_data_2)


## Data Cleaning
# Clean names
full.HOF.dat$Name<-gsub("X-", "", full.HOF.dat$Name)
full.HOF.dat$Name<-gsub("HOF", "", full.HOF.dat$Name)
full.HOF.dat$Name<-trimws(full.HOF.dat$Name)

full.HOF.dat<-full.HOF.dat %>%
  group_by(Name) %>%
  filter(row_number() == 1) %>%
  ungroup()

#QC
length(unique(full.HOF.dat$Name)) == nrow(full.HOF.dat)
full.HOF.dat$Name
full.HOF.dat$Name<-trimws(full.HOF.dat$Name)
full.HOF.dat.temp<-separate_wider_delim(full.HOF.dat, "Name", delim = " ",names = c("First", "Last"), too_many = "merge", cols_remove=FALSE)
full.HOF.dat.temp$Last[which(full.HOF.dat.temp$Last=="PeÃ±a")]<-"Pena" #fixing one name

full.HOF.dat<-full.HOF.dat.temp

#### Split into Batter and Pitcher Data Frames ####
table(full.HOF.dat$HOF) # 73 HOF, 403 non

HOF_P.dat<-full.HOF.dat%>%
  filter(G_Pit>50)
HOF_H.dat<-full.HOF.dat%>%
  filter((G_Pit<50 | is.na(G_Pit)) & G_Hit>425)

# Remove Hitting Stats for pitchers and pitching stats for hitters
names(HOF_H.dat)
HOF_H.dat<-HOF_H.dat%>%
  select(-c(W, L, ERA, `ERA+`, `WHIP`, G_Pit, GS, SV, IP, H_Pit, HR_Pit, BB_Pit, SO))
names(HOF_P.dat)
HOF_P.dat<-HOF_P.dat%>%
  select(-c(G_Hit, AB, R, H_Hit, HR_Hit, RBI, SB, BB_Hit, BA, OBP, SLG, OPS, `OPS+`))
table(HOF_H.dat$HOF)
table(HOF_P.dat$HOF)

## Model Hitters First
str(HOF_H.dat)
# Base Model
mod1.H.dat<-HOF_H.dat%>%
  select(-c(Rk, First, Last))
unique(is.na(mod1.H.dat)) #confirm no NA's
# Fit model
model1<-glm(`HOF`~.,data=mod1.H.dat[,-1], family = 'binomial')
summary(model1)

# Prediction
mod1.H.dat$predict_1<-predict(model1, newdata = mod1.H.dat, type='response')
mod1.H.dat$predict_1<-ifelse(mod1.H.dat$predict_1>0.5,1,0)
table(mod1.H.dat$predict_1)
table(mod1.H.dat$HOF)
table(Prediction = mod1.H.dat$predict_1, Actual = mod1.H.dat$HOF)
confusionMatrix(as.factor(mod1.H.dat$predict_1), as.factor(mod1.H.dat$HOF))

## Perform Variable selection veia Stepwise ANOVA
# Fit a full model with all predictors
mod1.H.dat$predict_1<-NULL
full_model <- lm(`HOF`~ ., data = mod1.H.dat[,-1])

# Perform stepwise ANOVA using the step function
stepwise_model <- step(full_model)

# Display the final model
summary(stepwise_model)

# Prediction
mod1.H.dat$predict<-predict(stepwise_model, newdata = mod1.H.dat, type='response')
mod1.H.dat$predict<-ifelse(mod1.H.dat$predict>0.31,1,0)
table(mod1.H.dat$predict)
table(mod1.H.dat$HOF)
table(Prediction = mod1.H.dat$predict, Actual = mod1.H.dat$HOF)
confusionMatrix(as.factor(mod1.H.dat$predict), as.factor(mod1.H.dat$HOF))
sensitivity(as.factor(mod1.H.dat$predict),as.factor(mod1.H.dat$HOF))+specificity(as.factor(mod1.H.dat$predict),as.factor(mod1.H.dat$HOF)) #0.31 TH is best

### Using Variable Importance
#Lasso regression
mod1.H.dat<-HOF_H.dat%>%
  select(-c(Rk, First, Last))
# Fit a glmnet model (lasso regression)
mod1.H.dat$HOF<-as.factor(mod1.H.dat$HOF)
glmnet_model <- train(
  `HOF` ~ .,
  data = mod1.H.dat[,-1],
  method = "glmnet",
  trControl = trainControl(method = "cv"),
  tuneLength = 10
)

# Display variable importance
varImp(glmnet_model)
# BA, OBP, YRS, WAR7, JAWS, HOFs, WAR
# Fit a binomial logistic regression model
logistic_model <- glm(HOF ~ BA + OBP + Yrs + WAR7 + JAWS + HOFs + WAR, data = mod1.H.dat[,-1], family = "binomial")

# Display model summary
summary(logistic_model)

# Prediction
mod1.H.dat$predict<-predict(logistic_model, newdata = mod1.H.dat[,-1], type='response')
mod1.H.dat$predict<-ifelse(mod1.H.dat$predict>0.3,1,0)
table(mod1.H.dat$predict)
table(mod1.H.dat$HOF)
table(Prediction = mod1.H.dat$predict, Actual = mod1.H.dat$HOF)
confusionMatrix(as.factor(mod1.H.dat$predict), as.factor(mod1.H.dat$HOF))
sensitivity(as.factor(mod1.H.dat$predict),as.factor(mod1.H.dat$HOF))+specificity(as.factor(mod1.H.dat$predict),as.factor(mod1.H.dat$HOF)) #0.31 TH is best

