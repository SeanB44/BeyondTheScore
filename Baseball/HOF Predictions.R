library(dplyr)
library(readxl)
library(caret)
library(openxlsx)
library(MASS)
### Read in data
# Get workbook names
wb<-loadWorkbook("./Data/HOF PRedictions.xlsx")
(sheets<-getSheetNames("./Data/HOF PRedictions.xlsx"))

bat.dat<-read_xlsx("./Data/HOF PRedictions.xlsx", sheet = "Batting")
pitch.dat<-read_xlsx("./Data/HOF PRedictions.xlsx", sheet = "Pitching")
new.bat.dat<-read_xlsx("./Data/HOF PRedictions.xlsx", sheet = "Upcoming H")
new.pitch.dat<-read_xlsx("./Data/HOF PRedictions.xlsx", sheet = "Upcoming P")

# Get necessary variables
bat.dat<-bat.dat %>% dplyr::select(c(Name,
                              WAR,`WAR/162`,H,PA,RBI,Bat,G,AVG,wOBA,`wRC+`,`AVG+`,OPS,`SLG+`,OBP,`OBP+`,HR,Fld,SB,`BB%`,BsR,Def,`K%`,
                              HOF, Indicator)) %>% # No Clutch, WPA, UBR, wSB,K%, AB or SLG(SLG removed due to singularity in model)
  filter(!is.na(Indicator))%>%
  dplyr::select(-Indicator)# %>%
  #na.omit()

pitch.dat<-pitch.dat %>% dplyr::select(c(Name,
                                  WAR,`WAR/162`,RAR,`W/SV`,`WHIP+`,ERA,`W%`,WHIP,FIP,	`ERA-`,`FIP-`,`AVG+`,`BB/9`,`AVG`,`K/9`,`SP/R`,
                                  ,HOF, Indicator)) %>% # No xFIP, xFIP-,xERA, SIERA, W, L, G, GS, SP/R
  filter(!is.na(Indicator)) %>%
  dplyr::select(-Indicator)
colSums(is.na(bat.dat))
bat.dat<-na.omit(bat.dat)
colSums(is.na(pitch.dat))

pitch.dat$HOF<-as.factor(pitch.dat$HOF)
bat.dat$HOF<-as.factor(bat.dat$HOF)


## Logistic Regression Model
# Split data
set.seed(23)
batSplit<-createDataPartition(bat.dat$HOF, p = 0.75, list = FALSE)
train.bat.dat<-bat.dat[batSplit,]
test.bat.dat<-bat.dat[-batSplit,]

pitSplit<-createDataPartition(pitch.dat$HOF, p = 0.7, list = FALSE)
train.pitch.dat<-pitch.dat[pitSplit,]
test.pitch.dat<-pitch.dat[-pitSplit,]

# Batting Model

bat.mod<-glm(HOF ~ ., data = train.bat.dat[,-1], family = binomial("logit"))%>%
  stepAIC(trace = FALSE)

summary(bat.mod)

# Pitching Model
pitch.mod<-glm(HOF ~ ., data = train.pitch.dat[,-1], family = binomial("logit"))%>%
  stepAIC(trace = FALSE)
summary(pitch.mod)

## Predict
# Batting
bat.pred<-predict(bat.mod, newdata = test.bat.dat[,-1], type = "response")
bat.pred<-bat.pred+.15
bat.pred<-ifelse(bat.pred > 1, 1, bat.pred)
bat.pred<-ifelse(bat.pred > 0.5, 1, 0)
confusionMatrix(as.factor(bat.pred), test.bat.dat$HOF)

# Pitching
pit.pred<-predict(pitch.mod, newdata = test.pitch.dat[,-1], type = "response")
pit.pred<-pit.pred+.29
pit.pred<-ifelse(pit.pred > 1, 1, pit.pred)
pit.pred<-ifelse(pit.pred > 0.5, 1,0)
confusionMatrix(as.factor(pit.pred), test.pitch.dat$HOF)


## Rescale 
rescale <- function(x, old_min, old_max, new_min, new_max) {
  ((x - old_min) / (old_max - old_min)) * (new_max - new_min) + new_min
}
## Variable Importance
# Batting
bat.importance<-varImp(bat.mod)/sum(varImp(bat.mod))
pit.importance<-varImp(pitch.mod)/sum(varImp(pitch.mod))
varImp.list<-list(
  `Batter Variable Importance` = data.frame(bat.importance),
  `Pitching Variable Importance` = data.frame(pit.importance)
)
write.xlsx(varImp.list, "./Output/HOF Variable Importance.xlsx", rowNames = TRUE)

## Run Model on All data
# Batting
full.bat.pred<-predict(bat.mod, newdata = bat.dat[,-1], type = "response")
full.bat.pred<-ifelse(full.bat.pred > .35, rescale(full.bat.pred, .35, 1, .50, 1.00), 
       rescale(full.bat.pred, 0, .35, 0, .50))
full.bat.pred.raw<-full.bat.pred
full.bat.pred<-ifelse(full.bat.pred > 1, 1, full.bat.pred)
full.bat.pred<-ifelse(full.bat.pred > 0.5, 1, 0)
confusionMatrix(as.factor(full.bat.pred), bat.dat$HOF)

batter_preds.dat<-data.frame(Name = bat.dat$Name, HOF = bat.dat$HOF, Prediction = full.bat.pred, `Raw Probability`= full.bat.pred.raw)

# Pitching
full.pit.pred<-predict(pitch.mod, newdata = pitch.dat[,-1], type = "response")
full.pit.pred<-ifelse(full.pit.pred > .21, rescale(full.pit.pred, .21, 1, .50, 1.00), 
                      rescale(full.pit.pred, 0, .21, 0, .50))
full.pit.pred.raw<-full.pit.pred
full.pit.pred<-ifelse(full.pit.pred > 1, 1, full.pit.pred)
full.pit.pred<-ifelse(full.pit.pred > 0.5, 1,0)
confusionMatrix(as.factor(full.pit.pred), pitch.dat$HOF)
pitcher_preds.dat<-data.frame(Name = pitch.dat$Name, HOF = pitch.dat$HOF, Prediction = full.pit.pred, `Raw Probability`= full.pit.pred.raw)

# Combine
all_preds.dat<-rbind(batter_preds.dat, pitcher_preds.dat)

# Predict Players On ballot or coming up
# Batting
new.bat.pred<-predict(bat.mod, newdata = new.bat.dat[,-1], type = "response")
new.bat.pred<-ifelse(new.bat.pred > .35, rescale(new.bat.pred, .35, 1, .50, 1.00), 
                      rescale(new.bat.pred, 0, .35, 0, .50))
new.bat.pred.raw<-new.bat.pred
new.bat.pred<-ifelse(new.bat.pred > 1, 1, new.bat.pred)
new.bat.pred<-ifelse(new.bat.pred > 0.5, 1, 0)
new.batter_preds.dat<-data.frame(Name = new.bat.dat$Name, HOF = new.bat.dat$HOF, Prediction = new.bat.pred, `Raw Probability`= new.bat.pred.raw)


# Pitching
new.pit.pred<-predict(pitch.mod, newdata = new.pitch.dat[,-1], type = "response")
new.pit.pred<-ifelse(new.pit.pred > .21, rescale(new.pit.pred, .21, 1, .50, 1.00), 
                      rescale(new.pit.pred, 0, .21, 0, .50))
new.pit.pred.raw<-new.pit.pred
new.pit.pred<-ifelse(new.pit.pred > 1, 1, new.pit.pred)
new.pit.pred<-ifelse(new.pit.pred > 0.5, 1,0)
new.pitcher_preds.dat<-data.frame(Name = new.pitch.dat$Name, HOF = new.pitch.dat$HOF, Prediction = new.pit.pred, `Raw Probability`= new.pit.pred.raw)

# Combine
new_preds.dat<-rbind(new.batter_preds.dat, new.pitcher_preds.dat)

# Export
write.xlsx(all_preds.dat, "./Output/HOF Probabilities v4.xlsx", rowNames = F, overwrite = T)
write.xlsx(new_preds.dat, "./Output/Future HOF Probabilities v4.xlsx", rowNames = F, overwrite = T)
