source('../../Functions.R')

####### PREDICTING PLAYOFF SUCCESS #####
### Load Data ###
raw.dat<-read.xlsx('./Data/Standings Analysis.xlsx', sheet = "2000-2023",check.names = F, sep.names = "_")
names(raw.dat)

## Data Cleaning
# Initial cleaning of unneeded variables and NAs
raw.dat<-raw.dat%>%
  filter(!is.na(`Post-Season_Wins`))

## Data Transformations ##
raw.dat$pythW<-as.numeric(c(unlist(str_split(raw.dat$pythWL, "-"))[seq(from=1, to=500, by = 2)]))
raw.dat$pythL<-as.numeric(c(unlist(str_split(raw.dat$pythWL, "-"))[seq(from=2, to=500, by = 2)]))
raw.dat$`pythWL%`<-raw.dat$pythW / (raw.dat$pythW + raw.dat$pythL)

# Select Necessary Columns
full.dat.DRS_non<-raw.dat %>%
  dplyr::select(c("Year", "Tm", "W-L%", "R", "RA", "Rdiff",
                  "SRS", "Luck", "AVG", "wOBA", "SLG", "WAR-B",
                  "ERA", "K/9", "FIP", "WAR-P", "Post-Season_Wins"))
full.dat.DRS<-raw.dat %>%
  dplyr::select(c("Year", "Tm", "W-L%", "R", "RA", "Rdiff",
                  "SRS", "Luck", "AVG", "wOBA", "SLG", "WAR-B",
                  "ERA", "K/9", "FIP", "WAR-P", "DRS","Post-Season_Wins"))%>%
  na.omit() %>%
  filter(DRS!=0)

## Specify Data that will be used
full.dat<-full.dat.DRS

# Set IVs
IVs<-names(full.dat)[-c(1:2, ncol(full.dat))]
IVs<-gsub("`", "", IVs)
DV<-"Post-Season_Wins"

# Fit Initial full model
base.model<-lm(formula = `Post-Season_Wins` ~., data = full.dat[-c(1:2)])
cor(base.model$model)
vif(base.model)

# ## Variable selection
# stepmodel <- step(lm(base.model, data = full.dat[,-c(1:2)]), direction = "both", k = 2, trace = 0)
# names(stepmodel$coefficients)
# names(full.dat[, -c(1:2)])[!names(full.dat[, -c(1:2)]) %in%names(stepmodel$coefficients)]
# vif(stepmodel)
# #
# IVs<-names(stepmodel$coefficients)[-1]

summary(lm(formula = `Post-Season_Wins` ~ `W-L%`, data = full.dat[-c(1:2)]))
summary(lm(formula = `Post-Season_Wins` ~ Rdiff, data = full.dat[-c(1:2)]))
summary(lm(formula = `Post-Season_Wins` ~ `R`, data = full.dat[-c(1:2)]))
summary(lm(formula = `Post-Season_Wins` ~ `RA`, data = full.dat[-c(1:2)]))
summary(lm(formula = `Post-Season_Wins` ~ `SRS`, data = full.dat[-c(1:2)]))
summary(lm(formula = `Post-Season_Wins` ~ `Luck`, data = full.dat[-c(1:2)]))
summary(lm(formula = `Post-Season_Wins` ~ `wOBA`, data = full.dat[-c(1:2)]))
summary(lm(formula = `Post-Season_Wins` ~ `WAR-B`, data = full.dat[-c(1:2)]))
summary(lm(formula = `Post-Season_Wins` ~ `ERA`, data = full.dat[-c(1:2)]))
summary(lm(formula = `Post-Season_Wins` ~ `FIP`, data = full.dat[-c(1:2)]))
summary(lm(formula = `Post-Season_Wins` ~ `WAR-P`, data = full.dat[-c(1:2)]))
summary(lm(formula = `Post-Season_Wins` ~ `DRS`, data = full.dat[-c(1:2)]))

IVs.full <- c("W-L%", "Rdiff", "R", "SRS", "Luck", "wOBA", "WAR-B", "ERA", "FIP", "WAR-P", "DRS")
IVs.sig <- c("W-L%", "Rdiff", "R",  "SRS", "wOBA", "WAR-B")
IVs.reduced<-c("W-L%", "Rdiff", "WAR-B", "WAR-P", "DRS")

IVs <- IVs.full
IVs <- IVs.sig
IVs <- IVs.reduced

full.dat<-full.dat %>%
  dplyr::select(c(Year, Tm, all_of(IVs), DV))

# Train/Test Split
set.seed(888)
trainIndex <- createDataPartition(full.dat$`Post-Season_Wins`, p = .667, 
                                  list = FALSE, 
                                  times = 1)
train.dat <- full.dat[ trainIndex,]
test.dat  <- full.dat[-trainIndex,]

# Random Forest
table(train.dat$`Post-Season_Wins`)
table(test.dat$`Post-Season_Wins`)

lm.model<-lm(formula = `Post-Season_Wins`~., data = train.dat[-c(1:2)])
summary(lm.model)
vif(lm.model)
plot(lm.model)

# statistical test for linearity

plot(lm.model$fitted.values, residuals(lm.model))
abline(0, 0, col = "red")

# Save coefficients
coef_df <- data.frame(coef(lm.model), check.names = F)

## Make Predictions on test set
predicted_wins <- predict(lm.model, newdata = test.dat)

# Calculate MAE
MAE <- mean(abs(predicted_wins - test.dat$`Post-Season_Wins`))
MAE 
# Reduced Set = 0.84

# Plot predictions vs. Actual
plot(test.dat$`Post-Season_Wins`, predicted_wins, xlab = "Actual Wins", ylab = "Predicted Wins")
abline(0, 1, col = "red")

# Export PRedictions vs. Actual
write.xlsx(data.frame("Actual Post-Season Performance" = test.dat$`Post-Season_Wins`, "Predicted Post-Season Performance" = predicted_wins), "Post-Season Predictions -Reduced IV Set.xlsx", sheetName = "Reduced IV Set", rowNames = F)
