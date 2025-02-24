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
# may.dat<-read_xlsx("./Data/May 1st Baseball Standing Analysis.xlsx", sheet = 1)
# eos.dat<-read_xlsx("./Data/May 1st Baseball Standing Analysis.xlsx", sheet = 2)
# names(may.dat)
# names(eos.dat)
# names(may.dat)<-c("Year", "Team", "May 1st W", "May 1st L", "May 1st WP%", "May 1st GB", "May 1st RS", "May 1st RA", "May 1st Run Diff", "May 1st Pythag WP%")
# names(eos.dat)<-c("Year", "Team", "EOS W", "EOS L", "EOS WP%", "EOS GB", "EOS RS", "EOS RA", "EOS Run Diff", "EOS Pythag WP%")
# ## Join the two data sets
# full.dat<-left_join(may.dat, eos.dat, by = c("Year", "Team"))%>%
#   select(-c("May 1st W", "May 1st L","EOS W", "EOS L", "May 1st RS", "May 1st RA", "EOS RS", "EOS RA"))
# names(full.dat)

full.dat<-read_xlsx( "./Data/May1 vs End of Season Standings data.xlsx",sheet = 1)
## Perform paired samples t-test on May vs End of season WP
t.test(full.dat$`May 1st WP%`, full.dat$`EOS WP%`, alternative = "two.sided", paired = TRUE) #p-value = 0.9216, Mean difference of 0.00038

## Perform paired samples t-test on May vs End of season Pythagorean WP
t.test(full.dat$`May 1st Pythag WP%`, full.dat$`EOS WP%`, alternative = "two.sided", paired = TRUE) #p-value = 0.985, Mean difference of 0.00006

## Perform paired samples t-test on May vs End of season Run Differential
t.test(full.dat$`May 1st Run Diff`, full.dat$`EOS Run Diff`, alternative = "two.sided", paired = TRUE) #p-value = 0.896, Mean difference of 0.0045

## Examine data
plot(full.dat$`May 1st WP%`, full.dat$`EOS WP%`, xlab = "May 1st WP%", ylab = "End of Season WP%", main = "May 1st WP% vs End of Season WP%")
abline(lm(full.dat$`EOS WP%`~full.dat$`May 1st WP%`), col = "red")

hist(full.dat$`May 1st WP%`, main = "May 1st WP%", xlab = "WP%", col = "blue")
hist(full.dat$`EOS WP%`, main = "End of Season WP%", xlab = "WP%", col = "red")

plot(full.dat$`May 1st Pythag WP%`, full.dat$`EOS Pythag WP%`, xlab = "May 1st Pythag WP%", ylab = "End of Season Pythag WP%", main = "May 1st Pythag WP% vs End of Season Pythag WP%")
abline(lm(full.dat$`EOS Pythag WP%`~full.dat$`May 1st Pythag WP%`), col = "red")

plot(full.dat$`May 1st Run Diff`, full.dat$`EOS Run Diff`, xlab = "May 1st Run Diff", ylab = "End of Season Run Diff", main = "May 1st Run Diff vs End of Season Run Diff")

hist(full.dat$`May 1st Run Diff`, main = "Run Differential", xlab = "Year", col = "green")
hist(full.dat$`EOS Run Diff`, main = "Run Differential", xlab = "Year", col = "yellow")

## Split into training and testing data
set.seed(101)
split<-sample.split(full.dat$`EOS WP%`, SplitRatio = 0.75)
train.dat<-subset(full.dat, split == TRUE)
test.dat<-subset(full.dat, split == FALSE)

## Fit a linear regression model - just WP
lm.fit<-lm(`EOS WP%`~`May 1st WP%`, data = train.dat)
summary(lm.fit) #R2 = 0.33
# Save summary
WPct.model<-summary(lm.fit)
# # Check assumptions
# plot(lm.fit)
# Make predictions
lm.pred<-predict(lm.fit, newdata = test.dat)
# Calculate MEA
mean(abs(lm.pred - test.dat$`EOS WP%`)) #0.048 point average absolute error
0.047*162
90/162 - 82/162

## Fit a linear regression model - just Pythag WP
lm.fit<-lm(`EOS WP%`~`May 1st Pythag WP%`, data = train.dat)
summary(lm.fit) #r2=.33
# Save summary
PythagWPct.model<-summary(lm.fit)
# # Check assumptions
# plot(lm.fit)
# Make predictions
lm.pred<-predict(lm.fit, newdata = test.dat)
# Calculate MEA
mean(abs(lm.pred - test.dat$`EOS WP%`)) #0.04 point average absolute error
0.043*162 #7.0 games
90/162 - 79/162

## Fit a linear regression model
lm.fit<-lm(`EOS WP%`~`May 1st WP%`+`May 1st Run Diff`, data = train.dat)
summary(lm.fit) #r2=.367
# Save summary
WPct_Rdiff.model<-summary(lm.fit)
# Check assumptions
plot(lm.fit)
# Make predictions
lm.pred<-predict(lm.fit, newdata = test.dat)
# Calculate MEA
mean(abs(lm.pred - test.dat$`EOS WP%`)) #0.046 point average absolute error
0.046*162
90/162 - 83/162 #7 games

## Fit a linear regression model - all variables
lm.fit<-lm(`EOS WP%`~`May 1st WP%`+`May 1st Run Diff`+`May 1st Pythag WP%`, data = train.dat)
summary(lm.fit) #.366
# Save summary
All.model<-summary(lm.fit)
# Check assumptions
plot(lm.fit)
# Make predictions
lm.pred<-predict(lm.fit, newdata = test.dat)
# Calculate MEA
mean(abs(lm.pred - test.dat$`EOS WP%`)) #0.046 point average absolute error

## Fit a linear regression model - Just run diff
lm.fit<-lm(`EOS WP%`~`May 1st Run Diff`, data = train.dat)
summary(lm.fit) #0.340
# Save summary
Rdiff.model<-summary(lm.fit)
# Check assumptions
plot(lm.fit)
# Make predictions
lm.pred<-predict(lm.fit, newdata = test.dat)
# Calculate MEA
mean(abs(lm.pred - test.dat$`EOS WP%`)) #0.0012 point average absolute error

## Fit a linear regression model - Just run diff + Pythag
lm.fit<-lm(`EOS WP%`~`May 1st Run Diff`+`May 1st Pythag WP%`, data = train.dat)
summary(lm.fit) #0.338
# Save summary
Rdiff_Pythag.model<-summary(lm.fit)
# Check assumptions
plot(lm.fit)
# Make predictions
lm.pred<-predict(lm.fit, newdata = test.dat)
# Calculate MEA
mean(abs(lm.pred - test.dat$`EOS WP%`)) #0.045 point average absolute error


## Correlations
cor(full.dat$`May 1st WP%`, full.dat$`EOS WP%`) #0.565
cor(full.dat$`May 1st Pythag WP%`, full.dat$`EOS WP%`) #0.583
cor(full.dat$`May 1st Run Diff`, full.dat$`EOS WP%`) #0.584
cor(full.dat$`May 1st Run Diff`, full.dat$`EOS Run Diff`) #0.619

# League's Best Team
full.dat$`EOS GB`<-ifelse(full.dat$`EOS GB` == 0, 1, 0)
full.dat$`May 1st GB`<-ifelse(full.dat$`May 1st GB` == 0, 1, 0)
cor(full.dat$`May 1st GB`, full.dat$`EOS GB`) #.204 (LOW)

length(which(full.dat$`EOS GB` == 1 & full.dat$`May 1st GB` == 1))/length(which(full.dat$`EOS GB`==1)) #28.2% Best team in league finishes as best

write.xlsx(full.dat, "./Data/May1 vs End of Season Standings data.xlsx", rowNames = FALSE)
