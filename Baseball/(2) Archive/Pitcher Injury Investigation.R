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

### Read in Data ###
tj.dat<-read_xlsx("./Data/Tommy John Investigation.xlsx")
head(tj.dat)

## Velocity
velocity.dat<-list()
velocity.dat[[1]]<-read.csv("./Data/2017-Velocity.csv")
velocity.dat[[2]]<-read.csv("./Data/2018-Velocity.csv")
velocity.dat[[3]]<-read.csv("./Data/2019-Velocity.csv")
velocity.dat[[4]]<-read.csv("./Data/2020-Velocity.csv")
velocity.dat[[5]]<-read.csv("./Data/2021-Velocity.csv")
velocity.dat[[6]]<-read.csv("./Data/2022-Velocity.csv")
velocity.dat[[7]]<-read.csv("./Data/2023-Velocity.csv")

## Spin Rate
spin.dat<-list()
spin.dat[[1]]<-read.csv("./Data/2017-Average Spin.csv")
spin.dat[[2]]<-read.csv("./Data/2018-Average Spin.csv")
spin.dat[[3]]<-read.csv("./Data/2019-Average Spin.csv")
spin.dat[[4]]<-read.csv("./Data/2020-Average Spin.csv")
spin.dat[[5]]<-read.csv("./Data/2021-Average Spin.csv")
spin.dat[[6]]<-read.csv("./Data/2022-Average Spin.csv")
spin.dat[[7]]<-read.csv("./Data/2023-Average Spin.csv")

for(i in 1:7){
  velocity.dat[[i]]<-velocity.dat[[i]]%>%
    select(last_name..first_name, ff_avg_speed)%>%
    na.omit()
  spin.dat[[i]]<-spin.dat[[i]]%>%
    select(last_name..first_name, ff_avg_spin)%>%
    na.omit()
}

tempo.dat<-read.csv("./Data/pitch_tempo_min500.csv", check.names=F)

for(i in 1:length(velocity.dat)){
  for (j  in 1:length(velocity.dat[[i]][,1])) {
    velocity.dat[[i]]$last_name..first_name[j]<-paste0(strsplit(velocity.dat[[i]][,1][j], split = ", ")[[1]][2], " ", strsplit(velocity.dat[[i]][,1][j], split = ",")[[1]][1])
    
  }
}
for(i in 1:length(spin.dat)){
  for (j  in 1:length(spin.dat[[i]][,1])) {
    spin.dat[[i]]$last_name..first_name[j]<-paste0(strsplit(spin.dat[[i]][,1][j], split = ", ")[[1]][2], " ", strsplit(spin.dat[[i]][,1][j], split = ",")[[1]][1])
    
  }
}
for(i in 1:length(tempo.dat$entity_name)){
  tempo.dat$entity_name[i]<-paste0(strsplit(tempo.dat$entity_name[i], split = ", ")[[1]][2], " ", strsplit(tempo.dat$entity_name[i], split = ",")[[1]][1])
}
head(tj.dat)

Pitcher.dat<-list(
  "Injuries"= tj.dat,
  "2017 Velocity"= velocity.dat[[1]],
  "2018 Velocity"=velocity.dat[[2]],
  "2019 Velocity"=velocity.dat[[3]],
  "2020 Velocity"=velocity.dat[[4]],
  "2021 Velocity"=velocity.dat[[5]],
  "2022 Velocity"=velocity.dat[[6]],
  "2023 Velocity"=velocity.dat[[7]],
  "2017 Spin Rate"= spin.dat[[1]],
  "2018 Spin Rate"=spin.dat[[2]],
  "2019 Spin Rate"=spin.dat[[3]],
  "2020 Spin Rate"=spin.dat[[4]],
  "2021 Spin Rate"=spin.dat[[5]],
  "2022 Spin Rate"=spin.dat[[6]],
  "2023 Spin Rate"=spin.dat[[7]],
  "Pitch Tempo"=tempo.dat
)
#writexl::write_xlsx(Pitcher.dat, "./Data/Pitcher Data v2.xlsx")

#### Analysis ###
injured_pitcher.dat<-read.xlsx("./Data/Pitcher Data.xlsx", sheet = "Injuries")
all_velo.dat<-read.xlsx("./Data/Pitcher Data.xlsx", sheet = "All Velo")
all_spin.dat<-read.xlsx("./Data/Pitcher Data.xlsx", sheet = "All Spin Rate")
all_tempo.dat<-read.xlsx("./Data/Pitcher Data.xlsx", sheet = "Pitch Tempo")

### Fileter out injured pitchers from all pitcher dfs
all_velo.dat$Pitcher[all_velo.dat$Pitcher %in% injured_pitcher.dat$Player]
## Velo Data
all_velo.dat<-all_velo.dat%>%
  filter(!Pitcher %in% injured_pitcher.dat$Player)
# QC
all_velo.dat$Pitcher[all_velo.dat$Pitcher %in% injured_pitcher.dat$Player]

## Spin Rate Data
all_spin.dat$Pitcher[all_spin.dat$Pitcher %in% injured_pitcher.dat$Player]

all_spin.dat<-all_spin.dat%>%
  filter(!Pitcher %in% injured_pitcher.dat$Player)
# QC
all_spin.dat$Pitcher[all_spin.dat$Pitcher %in% injured_pitcher.dat$Player]

## Tempo Data
removal_idx<-!all_tempo.dat$entity_name %in% injured_pitcher.dat$Player

all_tempo.dat<-all_tempo.dat[removal_idx,]
# QC
all_tempo.dat$entity_name[all_tempo.dat$entity_name %in% injured_pitcher.dat$Player]

### Test Injured Pitcher Velo vs. Non Injured
mean(injured_pitcher.dat$Average.Velocity, na.rm = T) #94.65
mean(all_velo.dat$Velocity, na.rm = T) #93.24
# t-test
velo.t.test<-t.test(injured_pitcher.dat$Average.Velocity, all_velo.dat$Velocity, alternative = "greater")
# z-test
velo.z.test<-z.test(injured_pitcher.dat$Average.Velocity, all_velo.dat$Velocity, alternative = "greater",
                     sigma.x = sd(injured_pitcher.dat$Average.Velocity, na.rm = T), sigma.y = sd(all_velo.dat$Velocity, na.rm = T))
### Test Injured Pitcher spin rate vs. Non Injured
mean(injured_pitcher.dat$Average.Spin.Rate, na.rm = T) #2284.57
mean(all_spin.dat$ff_avg_speed, na.rm = T) #2254.35
# t-test
spin.t.test<-t.test(injured_pitcher.dat$Average.Spin.Rate, all_spin.dat$ff_avg_speed, alternative = "greater")
# z-test
spin.z.test<-z.test(injured_pitcher.dat$Average.Spin.Rate, all_spin.dat$ff_avg_speed, alternative = "greater",
                    sigma.x = sd(injured_pitcher.dat$Average.Spin.Rate, na.rm = T), sigma.y = sd(all_spin.dat$ff_avg_speed, na.rm = T))

### Test Injured Pitcher Tempo vs. Non Injured
mean(injured_pitcher.dat$Tempo, na.rm = T) #17.54
mean(all_tempo.dat$Average.Tempo, na.rm = T) #17.47
# t-test
tempo.t.test<-t.test(injured_pitcher.dat$Tempo, all_tempo.dat$Average.Tempo, alternative = "less")

## Join All Spin and Velo
all.dat<-left_join(all_velo.dat, all_spin.dat, by = "Pitcher")%>%
  group_by(Pitcher)%>%
  select(Pitcher, Velocity, ff_avg_speed)%>%
  aggregate(.~Pitcher, data = ., mean, na.rm = T)
all.pitcher.list<-list("Healthy Pitchers" = all.dat, 
     "TJ Pitchers" = injured_pitcher.dat)
## Export
writexl::write_xlsx(all.pitcher.list, "./Data/All Pitcher Data.xlsx")
