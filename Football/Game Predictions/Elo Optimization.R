source("../../../Football Functions.R")
library(readxl)
games.dat<-read_xlsx("./Output/CFB Prediction Tool.xlsb.xlsx", sheet = "Games") 
head(games.dat)
