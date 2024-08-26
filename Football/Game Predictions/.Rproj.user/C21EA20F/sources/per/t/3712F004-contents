source('../../../Football Functions.R')
source('./Point Differential Model.R')
### Model to use ###

summary(final.model)
PointDiff.model<-final.model
# source('./Win Probability Model.R')
# summary(prob.model)
# WinProb.model<-prob.model

# Load Workbook
wb<-loadWorkbook('./Output/CFB Prediction Tool - Week 1 - 2024 (2024-08-24).xlsx')

### Apply Function and Export Current Workbook ###
## Inputs
model<-PointDiff.model
#WinProb<-WinProb.model
year <-2024
wk<-1
date <- today()
wb<-wb
# Run Function
Gen_Tool_cfb(model, year, wk, date, wb = wb)             
