source('../../Functions.R')

####### PREDICTING PLAYOFF SUCCESS #####
### Load Data ###
raw.dat<-read.xlsx('./Data/Standings Analysis.xlsx', sheet = "2000-2023",check.names = F, sep.names = "_") 
names(raw.dat)

## Data Cleaning
# Initial cleaning of unneeded variables and NAs
raw.dat<-raw.dat%>%
 filter(!is.na(`Post-Season_Wins`))%>%
  dplyr::select(c("Year", "Tm", "W", "L", "W-L%", "R", "RA", "Rdiff", "SRS", "pythWL","Luck", "AVG", "wOBA", "WAR-B",
           "K/9", "ERA", "FIP", "WAR-P", "DRS", "Total_WAR"))

## Data Transformations ##
raw.dat$pythW<-as.numeric(c(unlist(str_split(raw.dat$pythWL, "-"))[seq(from=1, to=420, by = 2)]))
raw.dat$pythL<-as.numeric(c(unlist(str_split(raw.dat$pythWL, "-"))[seq(from=2, to=420, by = 2)]))
raw.dat$`pythWL%`<-raw.dat$pythW / (raw.dat$pythW + raw.dat$pythL)

# Re-arrange
names(full.dat)
full.dat<-full.dat[, c(1:(ncol(full.dat)-6), (ncol(full.dat)-2):ncol(full.dat), (ncol(full.dat)-5):(ncol(full.dat)-3))]
names(full.dat)

## Check normality of data
apply(full.dat[, 4:(ncol(full.dat)-2)], 2, shapiro.test)
apply(full.dat[, 4:(ncol(full.dat)-2)], 2, hist)

## Check for Linearity
for(i in 4:(ncol(full.dat)-2)){
  plot(full.dat$Rdiff~full.dat[, i], main = names(full.dat)[i])
}

## Correlation Matrix
cor_matrix <- data.frame(cor(full.dat[, 4:(ncol(full.dat)-2)]))
# Check for correlation coefficients
view(cor_matrix)
# Most correlated With Rdiff
sort(cor_matrix$Rdiff, decreasing = T)

### Modeling ###
model.dat<-full.dat
## Best Subsets Regression fo Find best group of predictors##
### MODEL 1: Best Subsets - identify best combination of predictors ####
regfit.full.BSS <-regsubsets(Rdiff~., model.dat[, 4:(ncol(model.dat)-2)], nvmax=35)
(reg.summary.BSS<-summary(regfit.full.BSS))
names(reg.summary.BSS)
which.min(reg.summary.BSS$cp) #Best-2
(mod1.best.cp<-names(which(reg.summary.BSS$which[2,]==TRUE))) # "`H_BsR/G`"   "Rdiff_Diff" 
min(reg.summary.BSS$cp) #-2.77
plot(reg.summary.BSS$cp, xlab = "Number of Variables",
     ylab = "Mallow's cp", type = "l") #2,3,4,5,6 all acceptable

which.min(reg.summary.BSS$bic) #Best-1
min(reg.summary.BSS$bic) #-4.62
(mod1.best.bic<-names(which(reg.summary.BSS$which[1,]==TRUE))) #"Rdiff_Diff" 
plot(reg.summary.BSS$bic, xlab = "Number of Variables",
     ylab = "Bayesian Information Criterion (BIC)", type = "l") #3, 3 are only viable ones

which.max(reg.summary.BSS$adjr2) #Best-16
(mod1.best.adjr2<-names(which(reg.summary.BSS$which[16,]==TRUE))) # "H_SP_xERA", "H_SP_xFIP", "H_SP_SIERA", "`H_P_Adj_WAR/G`",  
# "H_wOBA", "`H_BsR/G`","`H_Def/G`","`H_Off/G`", "`H_Adj_Bat_WAR/G`", "A_SPxERA", "A_SP_SIERA", "A_P_xFIP",          "A_wOBA"            "A_OPS"            
# "`A_Adj_Bat_WAR/G`", "Rdiff_Diff"       

max(reg.summary.BSS$adjr2) #0.055
reg.summary.BSS$adjr2[16]
plot(reg.summary.BSS$adjr2, xlab = "Number of Variables",
     ylab = "Adjusted RSq", type = "l")

base.model<-lm(formula = Rdiff~., data = model.dat[, 4:(ncol(model.dat)-2)])
summary(base.model) #All variables
stepmodel <- step(lm(base.model, data = model.dat[, 4:(ncol(model.dat)-2)]), direction = "both", k = 2, trace = 0)
names(stepmodel$coefficients)
names(model.dat[, 4:(ncol(model.dat)-2)])[!names(model.dat[, -c(1:3)]) %in% names(stepmodel$coefficients)]
vif(stepmodel)

summary(base.model)
summary(stepmodel)
step.names<-names(stepmodel$coefficients)[-1]

## Examine what variables are best ##
mod1.best.cp #Try
mod1.best.bic #Try
mod1.best.adjr2 #Try
step.names #Try

# ###  Best cp variables
# cp.step.lm<-lm(Rdiff~.,data=model.dat[, c(all_of(gsub("`", "",mod1.best.cp[-1])), "Rdiff")])
# summary(cp.step.lm) #Adj R2 = 0.055
# 
# forward.lm<-step(cp.step.lm, direction = "forward", k=2)
# summary(forward.lm) #Same as above
# 
# backward.lm<-step(cp.step.lm, direction = "backward", k=2)
# summary(backward.lm) #Same
# 
# names(cp.step.lm$coefficients)
# 
# ###  Best bic variables
# bic.step.lm<-lm(Rdiff~.,data=model.dat[, c(all_of(gsub("`", "",mod1.best.bic[-1])), "Rdiff")])
# summary(bic.step.lm) #Adj R2 = 0.045
# 
# forward.lm<-step(bic.step.lm, direction = "forward", k=2)
# summary(forward.lm) #Same as above
# 
# backward.lm<-step(bic.step.lm, direction = "backward", k=2)
# summary(backward.lm) #Same
# 
# names(bic.step.lm$coefficients)
# 
# ###  Best adjr2 variables
# adjr2.step.lm<-lm(Rdiff~.,data=model.dat[, c(all_of(gsub("`", "",mod1.best.adjr2[-1])), "Rdiff")])
# summary(adjr2.step.lm) #Adj R2 = 0.055
# 
# forward.lm<-step(adjr2.step.lm, direction = "forward", k=2)
# summary(forward.lm) #Same as above
# 
# backward.lm<-step(adjr2.step.lm, direction = "backward", k=2)
# summary(backward.lm) #worse
# 
# names(adjr2.step.lm$coefficients)


### Ridge Regression
library(glmnet)

# model.data<-model.dat[, c(all_of(gsub("`", "",mod1.best.cp[-1])), "Rdiff")] # Not viable
# model.data<-model.dat[, c(all_of(gsub("`", "",mod1.best.bic[-1])), "Rdiff")]
# model.data<-model.dat[, c(all_of(gsub("`", "",mod1.best.adjr2[-1])), "Rdiff")]
model.data<-model.dat[, c(all_of(gsub("`", "",step.names)), "Rdiff")]
model.data<-model.dat[, c("H_SP_xFIP", "A_SP_xFIP", "H_SP_SIERA", "A_SP_SIERA", "H_P_Adj_WAR/G", "A_P_Adj_WAR/G",
                          "H_wOBA", "A_wOBA", "H_BsR/G", "A_BsR/G", "H_Def/G", "A_Def/G", "H_Off/G", "A_Off/G",
                          "H_Adj_Bat_WAR/G", "A_Adj_Bat_WAR/G","Rdiff_Diff", "Rdiff")]

x<-model.matrix(Rdiff~.,model.data[, 1:(ncol(model.data))])
y<-model.data$Rdiff

# Get indices for train and test sets
#for(i in 551:600){
i<-184 #62.9%
#i<-113 #62.2%
#i<-584
set.seed(i)

subset<-sort(sample(1:nrow(model.data),nrow(model.data)*.67, replace = F ))
remain<-which(!(1:nrow(model.data))%in%subset)
train <- subset
test <- remain
y.test<-y[test]

cv.out<-cv.glmnet(x[train,],y[train], alpha=0, nfolds=10)
plot(cv.out)
bestlam<-cv.out$lambda.min
bestlam
# 12.00

ridge.mod<-glmnet(x,y,alpha=0,lambda = bestlam)
ridge.pred<-predict(ridge.mod,s=bestlam,newx=x[test,])
mean((ridge.pred-y.test)^2)
#Model cp: MSE=17.1
#Model bic: MSE=16.0
#Model adjr2: MSE=17.64
out<-glmnet(x,y, alpha = 0)
(coefs<-predict(out,type="coefficients",s=bestlam)[1:19]) #Note, ridge regression does not perform variable selection

# Mean Absolute Deviance
mean(abs(ridge.pred-y.test))
#cp = 3.15
#BIC = 3.11
#Adjr2 = 3.33
# Custom = 3.48

# Test Accuraccy in wins
test.set<-model.data[test,]
test.set$predicted<-ridge.pred
test.set$Predicted_home_win<-ifelse(test.set$predicted>0.025, 1, 0)
test.set$Result<-ifelse(test.set$Rdiff>0, 1, 0)
confusionMatrix(as.factor(test.set$Result), as.factor(test.set$Predicted_home_win)) #68.2%

home_pt_adv<- -0.025 #AdjR2
# None for cp
# None for bic

### Win Percentage
model.dat.logit<-model.data
model.dat.logit$Result<-ifelse(model.dat.logit$Rdiff>0, 1, 0)
model.dat.logit$Result<-factor(model.dat.logit$Result, levels = c(0,1), labels = c("Loss", "Win"))
model.dat.logit$Rdiff<-NULL
logit.mod<-glm(Result~., data=model.dat.logit[train,], family="binomial")
summary(logit.mod)

logit.mod$fitted.values
pred<-predict(logit.mod, newdata=model.dat.logit[test,], type="response")
predicted_result<-ifelse(pred>0.485,"Win","Loss") #seed = 184, home_adv = 0.015, Accuracy = 63.6%, 95% CI = 0.554 - 0.713
print(paste0("---- SEED = ", i," -----"))
print(confusionMatrix(as.factor(predicted_result), model.dat.logit[test,]$Result)) 
print(confusionMatrix(as.factor(predicted_result), model.dat.logit[test,]$Result)$overall["Accuracy"])
print("---------------------------------------------------------------------------")
#}
# Home Advantage
home_adv<- 0.03 #AdjR2

logit.mod$coefficients
## Export Coefficients
coefs<-data.frame(t(logit.mod$coefficients), check.names = F)
coefs<-cbind(coefs, home_adv)
write.xlsx(coefs, paste0("./Output/Win Probability Coefs - ",Sys.Date(), ".xlsx"))


### Variable Importance ###
varImpModel<-train(Result~., data=model.dat.logit[train,], method="glm",family="binomial")
importance<-varImp(varImpModel, scale = F)

# Extract importance values
importance_values <- importance$importance

# Sum of importance values
total_importance <- sum(importance_values)

# Calculate relative importance (in percentage)
relative_importance <- (importance_values / total_importance) * 100

# Print relative importance
relative_importance
coefs
variable_importance.dat<-data.frame("Variable" = names(coefs)[2:(ncol(coefs)-1)], "Importance" = relative_importance, check.names = F)

## Export Variable Importance
write.xlsx(variable_importance.dat, paste0("./Output/Variable Importance for Win Probability Model - ",Sys.Date(), ".xlsx"), rowNames = F)

