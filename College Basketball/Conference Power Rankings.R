## Libraries ##
# library(httr)
library(dplyr)
library(ggplot2)
library(e1071)
library(caret)
library(XML)
library(readxl)
library(tidyr)
library(tidyverse)
library(openxlsx)
library(hoopR)
library(RCurl)
library(rvest)
library(ISLR2)
library(leaps)
library(car)
library(pls)
library(relaimpo)
library(forecast)

source("../../Codes.R")
source("../../Functions.R")
login(user_email = Sys.getenv("KP_USER"), user_pw = Sys.getenv("KP_PW"))

### Get Conference Data ###
all_data <- kp_efficiency(min_year = 2001, max_year = 2025)
all_data <- all_data %>% dplyr::select(year, team,everything())
all_data$AdjEf<-all_data$adj_o - all_data$adj_d

# Clean up the data
View(all_data)
# check for columns with NAs
colSums(is.na(all_data))
# Remove NAs for seed - replace with 0
all_data$ncaa_seed[is.na(all_data$ncaa_seed)] <- 0

# Sort by Year then Conference, then Efficiency
all_conf <- all_data[order(-all_data$year, all_data$conf, all_data$AdjEf),]
head(all_conf)

# Rank AdjEf within each year
all_conf$AdjEf_rk <- ave(-all_conf$AdjEf, all_conf$year, FUN = rank)
head(all_conf)

# Calculate the number of teams in each conefrence each year
all_conf$n_teams <- as.numeric(ave(all_conf$team, all_conf$year, all_conf$conf, FUN = length))

# Aggregate conference data
all_conf<-all_conf %>% group_by(year, conf) %>%
  summarise(AdjEf = mean(AdjEf), AdjEf_rk = mean(AdjEf_rk), adj_o = mean(adj_o), adj_d = mean(adj_d),
            ncaa_bids = length(which(ncaa_seed>0)), n_teams =mean(n_teams))

all_conf$pct_bids<-all_conf$ncaa_bids/all_conf$n_teams

# Rank conferences by average efficiency
all_conf$AdjEf_rk <- ave(-all_conf$AdjEf, all_conf$year, FUN = rank)
all_conf$adj_o_rk <- ave(-all_conf$adj_o, all_conf$year, FUN = rank)
all_conf$adj_d_rk <- ave(-all_conf$adj_d, all_conf$year, FUN = rank)
all_conf$pct_bids_rk <- ave(-all_conf$pct_bids, all_conf$year, FUN = rank)

# Calculate if there is a relationship between conference efficiency and NCAA bids
cor(all_conf$AdjEf, all_conf$pct_bids)
cor(all_conf$adj_o, all_conf$pct_bids)
cor(all_conf$adj_d, all_conf$pct_bids)

lm1 <- lm(pct_bids ~ AdjEf, data = all_conf)
summary(lm1)

lm2 <- lm(pct_bids ~ adj_o, data = all_conf)
summary(lm2)

lm3 <- lm(pct_bids ~ adj_d, data = all_conf)
summary(lm3)

lm4<-lm(pct_bids ~ adj_o + adj_d, data = all_conf)
summary(lm4) ## Best ##

# Ranks lms
lm1_rk <- lm(pct_bids ~ AdjEf_rk, data = all_conf)
summary(lm1_rk)

lm2_rk <- lm(pct_bids ~ adj_o_rk, data = all_conf)
summary(lm2_rk)

lm3_rk <- lm(pct_bids ~ adj_d_rk, data = all_conf)
summary(lm3_rk)

lm4_rk<-lm(pct_bids ~ adj_o_rk + adj_d_rk, data = all_conf)
summary(lm4_rk)

# Overall Rankings
all_conf$overall_rk <- rank(-all_conf$AdjEf)
all_conf <- all_conf%>%dplyr::select(year, conf, overall_rk, AdjEf, adj_o, adj_d, n_teams, ncaa_bids,pct_bids,
                   AdjEf_rk, adj_o_rk, adj_d_rk, pct_bids_rk)

# remove 'ind' conference
all_conf <- all_conf[all_conf$conf != "ind",]

# Join with tournament advancement data
conf_tourney.dat<-read.xlsx("./Conference Power Rankings Data.xlsx", sheet = 1)
head(conf_tourney.dat)

all_conf<-left_join(all_conf, conf_tourney.dat[,c(1:2, (ncol(conf_tourney.dat)-8):ncol(conf_tourney.dat))], by = c("year", "conf"))

# Logit to predict Championship probability
logit1 <- glm(champ ~ AdjEf, data = all_conf, family = binomial)
summary(logit1)

new.data <- data.frame(AdjEf = all_conf$AdjEf[all_conf$year == 2025 & all_conf$conf == "SEC"])
predict(logit1, newdata = new.data, type = "response") #77.5% channce an SEC team will win the national championship

# Write to Excel
wb<-createWorkbook()
addWorksheet(wb, "Conference Power Rankings")
writeData(wb, "Conference Power Rankings", all_conf)
addWorksheet(wb, "All Data")
writeData(wb, "All Data", all_data)

saveWorkbook(wb, paste0("Conference Power Rankings - ", Sys.Date(), ".xlsx"), overwrite = TRUE)


