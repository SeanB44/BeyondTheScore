# install.packages("tidyverse", type = "binary")
# install.packages("ggrepel", type = "binary")
# install.packages("nflreadr", type = "binary")
# install.packages("nflplotR", type = "binary")
# install.packages("nflfastR", type = "binary")

library(tidyverse)
library(ggrepel)
library(nflreadr)
library(nflplotR)
library(nflfastR)
library(dplyr)
library(MASS)
library(caret)

options(scipen = 9999)
### Run code below if compiling new data ###

# years<-c(2005:2024)
# stats<-list()
# for (i in 1:length(years)){
#   stats[[i]]<-calculate_stats(years[i], "season", "team") %>% filter(season_type == "REG") %>%
#     select("team", "season","games", "passing_epa", "passing_cpoe", "rushing_epa", "receiving_epa", "penalty_yards", "fg_pct",
#            "sack_yards_lost", "passing_interceptions", "rushing_fumbles_lost", "sack_fumbles_lost", 
#            "fumble_recovery_opp", "def_interceptions", "def_sack_yards") %>%
#     mutate(passing_epa = passing_epa/games,
#            passing_cpoe = passing_cpoe/games,
#            rushing_epa = rushing_epa/games,
#            receiving_epa = receiving_epa/games,
#            penalty_yards = penalty_yards/games,
#            sack_yards_lost = sack_yards_lost/games,
#            passing_interceptions = passing_interceptions/games,
#            rushing_fumbles_lost = rushing_fumbles_lost/games,
#            sack_fumbles_lost = sack_fumbles_lost/games,
#            offensive_turnovers = (passing_interceptions + rushing_fumbles_lost + sack_fumbles_lost),
#            fumble_recovery_opp = fumble_recovery_opp/games,
#            def_interceptions = def_interceptions/games,
#            defensive_turnovers = (def_interceptions + fumble_recovery_opp),
#            def_sack_yards = def_sack_yards/games) %>%
#     mutate(id = paste0(team, "_", season))%>%
#     select(id, everything())
# }
# 
# # Unlist and stack
# stats<-do.call(rbind, stats)
# 
# standings<- rbind(
#   calculate_standings(fast_scraper_schedules(2024)) %>% select("season", "team","wins", "losses", "ties", "win_pct","sov", "sos", 
#                                                                "seed", "div_rank", "division") %>%
#     mutate(id = paste0(team, "_", season))%>%
#     select(id, everything()),
#   calculate_standings(fast_scraper_schedules(2023)) %>% select("season", "team","wins", "losses", "ties", "win_pct","sov", "sos", 
#                                                                          "seed", "div_rank", "division") %>%
#   mutate(id = paste0(team, "_", season))%>%
#   select(id, everything()),
#   calculate_standings(fast_scraper_schedules(2022)) %>% select("season", "team","wins", "losses", "ties", "win_pct","sov", "sos", 
#                                                                          "seed", "div_rank", "division") %>%
#   mutate(id = paste0(team, "_", season))%>%
#   select(id, everything()),
#   calculate_standings(fast_scraper_schedules(2021)) %>% select("season", "team","wins", "losses", "ties", "win_pct","sov", "sos", 
#                                                                          "seed", "div_rank", "division") %>%
#   mutate(id = paste0(team, "_", season))%>%
#   select(id, everything()),
#   calculate_standings(fast_scraper_schedules(2020)) %>% select("season", "team","wins", "losses", "ties", "win_pct","sov", "sos", 
#                                                                          "seed", "div_rank", "division") %>%
#   mutate(id = paste0(team, "_", season))%>%
#   select(id, everything()),
#   calculate_standings(fast_scraper_schedules(2019)) %>% select("season", "team","wins", "losses", "ties", "win_pct","sov", "sos", 
#                                                                          "seed", "div_rank", "division") %>%
#   mutate(id = paste0(team, "_", season))%>%
#   select(id, everything()),
#   calculate_standings(fast_scraper_schedules(2018)) %>% select("season", "team","wins", "losses", "ties", "win_pct","sov", "sos", 
#                                                                          "seed", "div_rank", "division") %>%
#   mutate(id = paste0(team, "_", season))%>%
#   select(id, everything()),
#   calculate_standings(fast_scraper_schedules(2017)) %>% select("season", "team","wins", "losses", "ties", "win_pct","sov", "sos", 
#                                                                          "seed", "div_rank", "division") %>%
#   mutate(id = paste0(team, "_", season))%>%
#   select(id, everything()),
#   calculate_standings(fast_scraper_schedules(2016)) %>% select("season", "team","wins", "losses", "ties", "win_pct","sov", "sos", 
#                                                                          "seed", "div_rank", "division") %>%
#   mutate(id = paste0(team, "_", season))%>%
#   select(id, everything()),
#   calculate_standings(fast_scraper_schedules(2015)) %>% select("season", "team","wins", "losses", "ties", "win_pct","sov", "sos", 
#                                                                          "seed", "div_rank", "division") %>%
#   mutate(id = paste0(team, "_", season))%>%
#   select(id, everything()),
#   calculate_standings(fast_scraper_schedules(2014)) %>% select("season", "team","wins", "losses", "ties", "win_pct","sov", "sos", 
#                                                                          "seed", "div_rank", "division") %>%
#   mutate(id = paste0(team, "_", season))%>%
#   select(id, everything()),
#   calculate_standings(fast_scraper_schedules(2013)) %>% select("season", "team","wins", "losses", "ties", "win_pct","sov", "sos", 
#                                                                          "seed", "div_rank", "division") %>%
#   mutate(id = paste0(team, "_", season))%>%
#   select(id, everything()),
#   calculate_standings(fast_scraper_schedules(2012)) %>% select("season", "team","wins", "losses", "ties", "win_pct","sov", "sos", 
#                                                                          "seed", "div_rank", "division") %>%
#   mutate(id = paste0(team, "_", season))%>%
#   select(id, everything()),
#   calculate_standings(fast_scraper_schedules(2011)) %>% select("season", "team","wins", "losses", "ties", "win_pct","sov", "sos", 
#                                                                          "seed", "div_rank", "division") %>%
#   mutate(id = paste0(team, "_", season))%>%
#   select(id, everything()),
#   calculate_standings(fast_scraper_schedules(2010)) %>% select("season", "team","wins", "losses", "ties", "win_pct","sov", "sos", 
#                                                                          "seed", "div_rank", "division") %>%
#   mutate(id = paste0(team, "_", season))%>%
#   select(id, everything()),
#   calculate_standings(fast_scraper_schedules(2009)) %>% select("season", "team","wins", "losses", "ties", "win_pct","sov", "sos", 
#                                                                          "seed", "div_rank", "division") %>%
#   mutate(id = paste0(team, "_", season))%>%
#   select(id, everything()),
#   calculate_standings(fast_scraper_schedules(2008)) %>% select("season", "team","wins", "losses", "ties", "win_pct","sov", "sos", 
#                                                                          "seed", "div_rank", "division") %>%
#   mutate(id = paste0(team, "_", season))%>%
#   select(id, everything()),
#   calculate_standings(fast_scraper_schedules(2007)) %>% select("season", "team","wins", "losses", "ties", "win_pct","sov", "sos", 
#                                                                          "seed", "div_rank", "division") %>%
#   mutate(id = paste0(team, "_", season))%>%
#   select(id, everything()),
#   calculate_standings(fast_scraper_schedules(2006)) %>% select("season", "team","wins", "losses", "ties", "win_pct","sov", "sos", 
#                                                                          "seed", "div_rank", "division") %>%
#   mutate(id = paste0(team, "_", season))%>%
#   select(id, everything()),
#   calculate_standings(fast_scraper_schedules(2005)) %>% select("season", "team","wins", "losses", "ties", "win_pct","sov", "sos", 
#                                                                          "seed", "div_rank", "division") %>%
#   mutate(id = paste0(team, "_", season))%>%
#   select(id, everything())
# )
# 
# conversion_rates<-rbind(
#   calculate_series_conversion_rates(fast_scraper_schedules(2024) %>%
#                                       slice_tail(n = 32) %>%
#                                       fast_scraper(), weekly = FALSE) %>%
#     select("season","team","off_scr_3rd", "def_scr_3rd") %>%
#     mutate(id = paste0(team, "_", season))%>%
#     select(id, everything()),
# calculate_series_conversion_rates(fast_scraper_schedules(2023) %>%
#                                     slice_tail(n = 32) %>%
#                                     fast_scraper(), weekly = FALSE) %>%
#   select("season","team","off_scr_3rd", "def_scr_3rd") %>%
#   mutate(id = paste0(team, "_", season))%>%
#   select(id, everything()),
# calculate_series_conversion_rates(fast_scraper_schedules(2022) %>%
#                                     slice_tail(n = 32) %>%
#                                     fast_scraper(), weekly = FALSE) %>%
#   select("season","team","off_scr_3rd", "def_scr_3rd") %>%
#   mutate(id = paste0(team, "_", season))%>%
#   select(id, everything()),
# calculate_series_conversion_rates(fast_scraper_schedules(2021) %>%
#                                     slice_tail(n = 32) %>%
#                                     fast_scraper(), weekly = FALSE) %>%
#   select("season","team","off_scr_3rd",  "def_scr_3rd") %>%
#   mutate(id = paste0(team, "_", season))%>%
#   select(id, everything()),
# calculate_series_conversion_rates(fast_scraper_schedules(2020) %>%
#                                     slice_tail(n = 32) %>%
#                                     fast_scraper(), weekly = FALSE) %>%
#   select("season","team","off_scr_3rd", "def_scr_3rd") %>%
#   mutate(id = paste0(team, "_", season))%>%
#   select(id, everything()),
# calculate_series_conversion_rates(fast_scraper_schedules(2019) %>%
#                                     slice_tail(n = 32) %>%
#                                     fast_scraper(), weekly = FALSE) %>%
#   select("season","team","off_scr_3rd", "def_scr_3rd") %>%
#   mutate(id = paste0(team, "_", season))%>%
#   select(id, everything()),
# calculate_series_conversion_rates(fast_scraper_schedules(2018) %>%
#                                     slice_tail(n = 32) %>%
#                                     fast_scraper(), weekly = FALSE) %>%
#   select("season","team","off_scr_3rd", "def_scr_3rd") %>%
#   mutate(id = paste0(team, "_", season))%>%
#   select(id, everything()),
# calculate_series_conversion_rates(fast_scraper_schedules(2017) %>%
#                                     slice_tail(n = 32) %>%
#                                     fast_scraper(), weekly = FALSE) %>%
#   select("season","team","off_scr_3rd", "def_scr_3rd") %>%
#   mutate(id = paste0(team, "_", season))%>%
#   select(id, everything()),
# calculate_series_conversion_rates(fast_scraper_schedules(2016) %>%
#                                     slice_tail(n = 32) %>%
#                                     fast_scraper(), weekly = FALSE) %>%
#   select("season","team","off_scr_3rd", "def_scr_3rd") %>%
#   mutate(id = paste0(team, "_", season))%>%
#   select(id, everything()),
# calculate_series_conversion_rates(fast_scraper_schedules(2015) %>%
#                                     slice_tail(n = 32) %>%
#                                     fast_scraper(), weekly = FALSE) %>%
#   select("season","team","off_scr_3rd", "def_scr_3rd") %>%
#   mutate(id = paste0(team, "_", season))%>%
#   select(id, everything()),
# calculate_series_conversion_rates(fast_scraper_schedules(2014) %>%
#                                     slice_tail(n = 32) %>%
#                                     fast_scraper(), weekly = FALSE) %>%
#   select("season","team","off_scr_3rd", "def_scr_3rd") %>%
#   mutate(id = paste0(team, "_", season))%>%
#   select(id, everything()),
# calculate_series_conversion_rates(fast_scraper_schedules(2013) %>%
#                                     slice_tail(n = 32) %>%
#                                     fast_scraper(), weekly = FALSE) %>%
#   select("season","team","off_scr_3rd", "def_scr_3rd") %>%
#   mutate(id = paste0(team, "_", season))%>%
#   select(id, everything()),
# calculate_series_conversion_rates(fast_scraper_schedules(2012) %>%
#                                     slice_tail(n = 32) %>%
#                                     fast_scraper(), weekly = FALSE) %>%
#   select("season","team","off_scr_3rd", "def_scr_3rd") %>%
#   mutate(id = paste0(team, "_", season))%>%
#   select(id, everything()),
# calculate_series_conversion_rates(fast_scraper_schedules(2011) %>%
#                                     slice_tail(n = 32) %>%
#                                     fast_scraper(), weekly = FALSE) %>%
#   select("season","team","off_scr_3rd", "def_scr_3rd") %>%
#   mutate(id = paste0(team, "_", season))%>%
#   select(id, everything()),
# calculate_series_conversion_rates(fast_scraper_schedules(2010) %>%
#                                     slice_tail(n = 32) %>%
#                                     fast_scraper(), weekly = FALSE) %>%
#   select("season","team","off_scr_3rd", "def_scr_3rd") %>%
#   mutate(id = paste0(team, "_", season))%>%
#   select(id, everything()),
# calculate_series_conversion_rates(fast_scraper_schedules(2009) %>%
#                                     slice_tail(n = 32) %>%
#                                     fast_scraper(), weekly = FALSE) %>%
#   select("season","team","off_scr_3rd", "def_scr_3rd") %>%
#   mutate(id = paste0(team, "_", season))%>%
#   select(id, everything()),
# calculate_series_conversion_rates(fast_scraper_schedules(2008) %>%
#                                     slice_tail(n = 32) %>%
#                                     fast_scraper(), weekly = FALSE) %>%
#   select("season","team","off_scr_3rd", "def_scr_3rd") %>%
#   mutate(id = paste0(team, "_", season))%>%
#   select(id, everything()),
# calculate_series_conversion_rates(fast_scraper_schedules(2007) %>%
#                                     slice_tail(n = 32) %>%
#                                     fast_scraper(), weekly = FALSE) %>%
#   select("season","team","off_scr_3rd", "def_scr_3rd") %>%
#   mutate(id = paste0(team, "_", season))%>%
#   select(id, everything()),
# calculate_series_conversion_rates(fast_scraper_schedules(2006) %>%
#                                     slice_tail(n = 32) %>%
#                                     fast_scraper(), weekly = FALSE) %>%
#   select("season","team","off_scr_3rd", "def_scr_3rd") %>%
#   mutate(id = paste0(team, "_", season))%>%
#   select(id, everything()),
# calculate_series_conversion_rates(fast_scraper_schedules(2005) %>%
#                                     slice_tail(n = 32) %>%
#                                     fast_scraper(), weekly = FALSE) %>%
#   select("season","team","off_scr_3rd", "def_scr_3rd") %>%
#   mutate(id = paste0(team, "_", season))%>%
#   select(id, everything())
# )
# 
# full.dat<-stats %>% left_join(standings, by = "id") %>% left_join(conversion_rates, by = "id") %>%
#   select(-team.y, -season.y, -season.x, -team.x) %>%
#   select(id, season, team, everything())
#   
# 
# sb_history.dat<-data.frame(
#   id = c("KC_2023","KC_2022", "LA_2021","TB_2020","KC_2019","NE_2018","PHI_2017","NE_2016","DEN_2015","NE_2014","SEA_2013","BAL_2012","NYG_2011","GB_2010","NO_2009","PIT_2008","NYG_2007","IND_2006","PIT_2005",
#          "SF_2023", "PHI_2022", "CIN_2021", "KC_2020", "SF_2019", "LA_2018", "NE_2017", "ATL_2016", "CAR_2015", "SEA_2014", "DEN_2013", "SF_2012", "NE_2011", "PIT_2010", "IND_2009", "ARI_2008", "NE_2007", "CHI_2006", "SEA_2005"),
#   Win_SB = c(rep(1, 19),rep(0, 19)),
#   Make_SB = rep(1, 38))
# 
# full.dat<-left_join(full.dat, sb_history.dat, by = "id")
# 
# full.dat$Win_SB<-ifelse(is.na(full.dat$Win_SB), 0 , full.dat$Win_SB)
# full.dat$Make_SB<-ifelse(is.na(full.dat$Make_SB), 0 , full.dat$Make_SB)
# 
# colSums(full.dat[, c(29,30)])
# 
# #writexl::write_xlsx(full.dat, "NFL team stats 2005-2024.xlsx")

full.dat<-openxlsx::read.xlsx("NFL team stats 2005-2024.xlsx")
colnames(full.dat)
colSums(is.na(full.dat))
## Clean data of missingness
full.dat$passing_cpoe<-NULL
full.dat$seed<-ifelse(is.na(full.dat$seed), 0, full.dat$seed)
full.dat<-na.omit(full.dat)

## Adjust some data points
full.dat$fumbles_lost<-full.dat$rushing_fumbles_lost + full.dat$sack_fumbles_lost
full.dat$sack_fumbles_lost<-NULL
full.dat$rushing_fumbles_lost<-NULL

# Stepwise variable selection for logistic regression
full.dat$Win_SB<-as.factor(full.dat$Win_SB)
full.dat$Make_SB<-as.factor(full.dat$Make_SB)
full.dat$seed<-as.factor(full.dat$seed)

# Split the data into training and test set
set.seed(40)
training.samples <- full.dat$Make_SB %>% 
  createDataPartition(p = 1, list = FALSE)
train.data  <- full.dat[training.samples, ]
test.data <- full.dat[-training.samples, ]

# Model
model <- glm(Make_SB ~., data = train.data %>% dplyr::select(-c("id", "team", "season", "games", 
                                                        "div_rank", "division", "Win_SB", "seed", "ties", "losses", "wins")),
             family = binomial) %>%
  stepAIC(trace = TRUE)
# Summarize the final selected model
summary(model)
# Make predictions
probabilities <- model %>% predict(train.data, type = "response")
predicted.classes <- ifelse(probabilities > 0.14, 1, 0)
# Model accuracy
mean(predicted.classes==train.data$Win_SB)
# Confusion matrix
table(train.data$Win_SB, predicted.classes)
# Calculate Sensitivity and specificity
sensitivity <- sum(predicted.classes[train.data$Win_SB == 1] == 1) / sum(train.data$Win_SB == 1)
specificity <- sum(predicted.classes[train.data$Win_SB == 0] == 0) / sum(train.data$Win_SB == 0)

