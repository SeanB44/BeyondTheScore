source('./Functions.R')

####### PREDICTING POINT DIFFERENTIAL #####
### Load Data ###
Batter.dat<-read.xlsx('./Data/Individual Season Data since 1947.xlsx', sheet = "All Batter", check.names = FALSE, sep.names="_")
SP.dat<-read.xlsx('./Data/Individual Season Data since 1947.xlsx', sheet = "SP", check.names = FALSE, sep.names="_")
RP.dat<-read.xlsx('./Data/Individual Season Data since 1947.xlsx', sheet = "RP", check.names = FALSE, sep.names="_")

## Data Cleaning
# Initial cleaning of variables and NA's
names(Batter.dat)
colSums(is.na(Batter.dat))
# Remove NA's
Batter.dat<-Batter.dat%>%
  na.omit()

names(SP.dat)
colSums(is.na(SP.dat))

names(RP.dat)
colSums(is.na(RP.dat))

## Correlation Matricies
dataframe.list<-list(Batter.dat, SP.dat, RP.dat)
names(dataframe.list)<-c("Batter", "SP", "RP")
correlation_matrix<-lapply(dataframe.list, function(x) data.frame(cor(x[, -c(1:3)]), check.names = F))

#write.xlsx(correlation_matrix, "./Output/WAR Correlation Matrix.xlsx", rowNames = T)

#### Modeling ####
## Batter
dataframe.list[[1]]$OPS<-NULL
for(j in 1:3){
for(i in 4:ncol(dataframe.list[[j]])){
  dataframe.list[[j]][,i]<-calculate_z_score(dataframe.list[[j]][,i])
}
}

batter.lm.full<-lm(dataframe.list[[1]]$WAR~., dataframe.list[[1]][, -c(1:3)])
sp.lm.full<-lm(dataframe.list[[2]]$WAR~., dataframe.list[[2]][, -c(1:3)])
rp.lm.full<-lm(dataframe.list[[3]]$WAR~., dataframe.list[[3]][, -c(1:3)])

summary(batter.lm.full)
summary(sp.lm.full)
summary(sp.lm.full)


##### Best Subsets Regression fo Find best group of predictors####
### MODEL 1: Best Subsets - identify best combination of predictors ####
regfit.full.Bat <-regsubsets(dataframe.list[[1]]$WAR~., dataframe.list[[1]][, -c(1:3)], nvmax=20)
(reg.summary.Bat<-summary(regfit.full.Bat))
names(reg.summary.Bat)

which.min(reg.summary.Bat$cp) #Best-11
(bat.best.cp<-names(which(reg.summary.Bat$which[9,]==TRUE)))
min(reg.summary.Bat$cp) #9.7
plot(reg.summary.Bat$cp, xlab = "Number of Variables",
     ylab = "Mallow's cp", type = "l")

which.min(reg.summary.Bat$bic) #Best-8
min(reg.summary.Bat$bic) #-9848
(bat.best.bic<-names(which(reg.summary.Bat$which[8,]==TRUE))) 
plot(reg.summary.Bat$bic, xlab = "Number of Variables",
     ylab = "Bayesian Information Criterion (BIC)", type = "l")

which.max(reg.summary.Bat$adjr2) #Best-11
(bat.best.adjr2<-names(which(reg.summary.Bat$which[11,]==TRUE)))
max(reg.summary.Bat$adjr2) #0.647
plot(reg.summary.Bat$adjr2, xlab = "Number of Variables",
     ylab = "Adjusted RSq", type = "l")

## Examine what variables are best ##
bat.best.bic
bat.best.cp 
bat.best.adjr2 
# Best 11
names(which(reg.summary.Bat$which[11,]==TRUE))
bss.names.bat<-c(names(which(reg.summary.Bat$which[11,]==TRUE)))[-1]
bss.names.bat<-gsub("`", "", bss.names.bat)
bss.names.bat<-c(bss.names.bat, "WAR")

which(names(dataframe.list[[1]]) %in% bss.names.bat)
names(dataframe.list[[1]])[which(names(dataframe.list[[1]]) %in% bss.names.bat)]

step.bat.lm<-lm(WAR~.,data=dataframe.list[[1]][, bss.names.bat])
summary(step.bat.lm) #Adj R2 = 0.647

step.new.lm<-step(step.bat.lm, direction = "both", k=2)
summary(step.new.lm) #Adj R2 = 0.370

var.names.bat<-names(dataframe.list[[1]])[which(names(dataframe.list[[1]]) %in% bss.names.bat)]

# Variable imporatnce
new.bat.dat<-dataframe.list[[1]]%>%
  dplyr::select(all_of(var.names.bat))

crf.bat <- ?calc.relimp(WAR~.,new.bat.dat, type = c("lmg"), rela = TRUE )
sort(crf.bat$lmg, decreasing = T)
batter.RI.df<-data.frame("Variable Importance" = sort(crf.bat$lmg, decreasing = T), check.names = F)
write.xlsx(batter.RI.df, "./Output/Batter Relative Importatnce to WAR.xlsx", rowNames = T)

### Ridge Regression ###
library(glmnet)

x<-model.matrix(WAR~.,new.bat.dat)
y<-new.bat.dat$WAR
# Get indices for train and test sets
set.seed(89)
subset<-sort(sample(1:nrow(new.bat.dat),nrow(new.bat.dat)*.6, replace = F ))
remain<-which(!(1:nrow(new.bat.dat))%in%subset)

train <- subset
test <- remain
y.test<-y[test]

cv.out<-cv.glmnet(x[train,],y[train], alpha=0, nfolds=50)
plot(cv.out)
bestlam<-cv.out$lambda.min
bestlam 
# 0.068

ridge.mod<-glmnet(x,y,alpha=0,lambda = bestlam)
ridge.pred<-predict(ridge.mod,s=bestlam,newx=x[test,])
mean((ridge.pred-y.test)^2)
# MSE=0.365

out<-glmnet(x,y, alpha = 0)
(coefs<-predict(out,type="coefficients",s=bestlam)[1:13,])


# Test Accuraccy
test.set<-new.bat.dat[test,]
test.set$predicted<-ridge.pred
mean(abs(test.set$predicted-test.set$WAR)) #MAD = 0.48 difference in WAR

############# STARTING PITCHER ############

##### Best Subsets Regression fo Find best group of predictors####
regfit.full.SP <-?regsubsets(dataframe.list[[2]]$WAR~., dataframe.list[[2]][, -c(1:3)], nvmax=20)
(reg.summary.SP<-summary(regfit.full.SP))
names(reg.summary.SP)

which.min(reg.summary.SP$cp) #Best-11
(SP.best.cp<-names(which(reg.summary.SP$which[9,]==TRUE)))
min(reg.summary.SP$cp) #9.2
plot(reg.summary.SP$cp, xlab = "Number of Variables",
     ylab = "Mallow's cp", type = "l")

which.min(reg.summary.SP$bic) #Best-8
min(reg.summary.SP$bic) #-6978.5
(SP.best.bic<-names(which(reg.summary.SP$which[8,]==TRUE))) 
plot(reg.summary.SP$bic, xlab = "Number of Variables",
     ylab = "Bayesian Information Criterion (BIC)", type = "l")

which.max(reg.summary.SP$adjr2) #Best-9
(SP.best.adjr2<-names(which(reg.summary.SP$which[9,]==TRUE)))
max(reg.summary.SP$adjr2) #0.716
plot(reg.summary.SP$adjr2, xlab = "Number of Variables",
     ylab = "Adjusted RSq", type = "l")

## Examine what variables are best ##
SP.best.bic
SP.best.cp 
SP.best.adjr2 
# Best 9
names(which(reg.summary.SP$which[9,]==TRUE))
bss.names.SP<-c(names(which(reg.summary.SP$which[9,]==TRUE)))[-1]
bss.names.SP<-gsub("`", "", bss.names.SP)
bss.names.SP<-c(bss.names.SP, "WAR")

which(names(dataframe.list[[2]]) %in% bss.names.SP)
names(dataframe.list[[2]])[which(names(dataframe.list[[2]]) %in% bss.names.SP)]

step.SP.lm<-lm(WAR~.,data=dataframe.list[[2]][, bss.names.SP])
summary(step.SP.lm) #Adj R2 = 0.716

step.new.lm<-step(step.SP.lm, direction = "both", k=2)
summary(step.new.lm) #Adj R2 = 0.716

var.names.SP<-names(dataframe.list[[2]])[which(names(dataframe.list[[2]]) %in% bss.names.SP)]

# Variable imporatnce
new.SP.dat<-dataframe.list[[2]]%>%
  dplyr::select(all_of(var.names.SP))

crf.SP <- calc.relimp(WAR~.,new.SP.dat, type = c("lmg"), rela = TRUE )
sort(crf.SP$lmg, decreasing = T)
SP.RI.df<-data.frame("Variable Importance" = sort(crf.SP$lmg, decreasing = T), check.names = F)
write.xlsx(SP.RI.df, "./Output/SP Relative Importatnce to WAR.xlsx", rowNames = T)

### Ridge Regression ###
library(glmnet)

x<-model.matrix(WAR~.,new.SP.dat)
y<-new.SP.dat$WAR
# Get indices for train and test sets
set.seed(89)
subset<-sort(sample(1:nrow(new.SP.dat),nrow(new.SP.dat)*.6, replace = F ))
remain<-which(!(1:nrow(new.SP.dat))%in%subset)

train <- subset
test <- remain
y.test<-y[test]

cv.out<-cv.glmnet(x[train,],y[train], alpha=0, nfolds=50)
plot(cv.out)
bestlam<-cv.out$lambda.min
bestlam 
# 0.069

ridge.mod<-glmnet(x,y,alpha=0,lambda = bestlam)
ridge.pred<-predict(ridge.mod,s=bestlam,newx=x[test,])
mean((ridge.pred-y.test)^2)
# MSE=0.304

out<-glmnet(x,y, alpha = 0)
(coefs<-predict(out,type="coefficients",s=bestlam)[1:11,])


# Test Accuraccy
test.set<-new.SP.dat[test,]
test.set$predicted<-ridge.pred
mean(abs(test.set$predicted-test.set$WAR)) #MAD = 0.43 difference in WAR


################### Relief Pitchers #########################

##### Best Subsets Regression fo Find best group of predictors####
regfit.full.RP <-regsubsets(dataframe.list[[3]]$WAR~., dataframe.list[[3]][, -c(1:3)], nvmax=20)
(reg.summary.RP<-summary(regfit.full.RP))
names(reg.summary.RP)

which.min(reg.summary.RP$cp) #Best-11
(RP.best.cp<-names(which(reg.summary.RP$which[10,]==TRUE)))
min(reg.summary.RP$cp) #11
plot(reg.summary.RP$cp, xlab = "Number of Variables",
     ylab = "Mallow's cp", type = "l")

which.min(reg.summary.RP$bic) #Best-9
min(reg.summary.RP$bic) #-7670.1
(RP.best.bic<-names(which(reg.summary.RP$which[9,]==TRUE))) 
plot(reg.summary.RP$bic, xlab = "Number of Variables",
     ylab = "Bayesian Information Criterion (BIC)", type = "l")

which.max(reg.summary.RP$adjr2) #Best-10
(RP.best.adjr2<-names(which(reg.summary.RP$which[10,]==TRUE)))
max(reg.summary.RP$adjr2) #0.634
plot(reg.summary.RP$adjr2, xlab = "Number of Variables",
     ylab = "Adjusted RSq", type = "l")

## Examine what variables are best ##
RP.best.bic
RP.best.cp 
RP.best.adjr2 
# Best 10
names(which(reg.summary.RP$which[10,]==TRUE))
bss.names.RP<-c(names(which(reg.summary.RP$which[10,]==TRUE)))[-1]
bss.names.RP<-gsub("`", "", bss.names.RP)
bss.names.RP<-c(bss.names.RP, "WAR")

which(names(dataframe.list[[3]]) %in% bss.names.RP)
names(dataframe.list[[3]])[which(names(dataframe.list[[3]]) %in% bss.names.RP)]

step.RP.lm<-lm(WAR~.,data=dataframe.list[[3]][, bss.names.RP])
summary(step.RP.lm) #Adj R2 = 0.716

step.new.lm<-step(step.RP.lm, direction = "both", k=2)
summary(step.new.lm) #Adj R2 = 0.6335

var.names.RP<-names(dataframe.list[[3]])[which(names(dataframe.list[[3]]) %in% bss.names.RP)]

# Variable imporatnce
new.RP.dat<-dataframe.list[[3]]%>%
  dplyr::select(all_of(var.names.RP))

crf.RP <- calc.relimp(WAR~.,new.RP.dat, type = c("lmg"), rela = TRUE )
sort(crf.RP$lmg, decreasing = T)
RP.RI.df<-data.frame("Variable Importance" = sort(crf.RP$lmg, decreasing = T), check.names = F)
write.xlsx(RP.RI.df, "./Output/RP Relative Importatnce to WAR.xlsx", rowNames = T)

### Ridge Regression ###
library(glmnet)

x<-model.matrix(WAR~.,new.RP.dat)
y<-new.RP.dat$WAR
# Get indices for train and test sets
set.seed(89)
subset<-sort(sample(1:nrow(new.RP.dat),nrow(new.RP.dat)*.6, replace = F ))
remain<-which(!(1:nrow(new.RP.dat))%in%subset)

train <- subset
test <- remain
y.test<-y[test]

cv.out<-cv.glmnet(x[train,],y[train], alpha=0, nfolds=50)
plot(cv.out)
bestlam<-cv.out$lambda.min
bestlam 
# 0.061

ridge.mod<-glmnet(x,y,alpha=0,lambda = bestlam)
ridge.pred<-predict(ridge.mod,s=bestlam,newx=x[test,])
mean((ridge.pred-y.test)^2)
# MSE=0.366

out<-glmnet(x,y, alpha = 0)
(coefs<-predict(out,type="coefficients",s=bestlam)[1:12,])


# Test Accuraccy
test.set<-new.RP.dat[test,]
test.set$predicted<-ridge.pred
mean(abs(test.set$predicted-test.set$WAR)) #MAD = 0.469 difference in WAR
