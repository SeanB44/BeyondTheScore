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
# 
# years<-c(2000:2024)
# stats<-list()
# for (i in 1:length(years)){
#   stats[[i]]<-calculate_stats(years[i], "season", "team") %>% filter(season_type == "REG") %>%
#     dplyr::select("team", "season","games", "passing_epa", "passing_cpoe", "rushing_epa", "receiving_epa", "penalty_yards", "fg_pct",
#                   "sack_yards_lost", "passing_interceptions", "rushing_fumbles_lost", "sack_fumbles_lost",
#                   "fumble_recovery_opp", "def_interceptions", "def_sack_yards") %>%
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
#     dplyr::select(id, everything())
# }
# 
# # Unlist and stack
# stats<-do.call(rbind, stats)
# 
# standings<- rbind(
#   calculate_standings(fast_scraper_schedules(2024)) %>% dplyr::select("season", "team","wins", "losses", "ties", "win_pct","sov", "sos",
#                                                                       "seed", "div_rank", "division") %>%
#     mutate(id = paste0(team, "_", season))%>%
#     dplyr::select(id, everything()),
#   calculate_standings(fast_scraper_schedules(2023)) %>% dplyr::select("season", "team","wins", "losses", "ties", "win_pct","sov", "sos",
#                                                                       "seed", "div_rank", "division") %>%
#     mutate(id = paste0(team, "_", season))%>%
#     dplyr::select(id, everything()),
#   calculate_standings(fast_scraper_schedules(2022)) %>% dplyr::select("season", "team","wins", "losses", "ties", "win_pct","sov", "sos",
#                                                                       "seed", "div_rank", "division") %>%
#     mutate(id = paste0(team, "_", season))%>%
#     dplyr::select(id, everything()),
#   calculate_standings(fast_scraper_schedules(2021)) %>% dplyr::select("season", "team","wins", "losses", "ties", "win_pct","sov", "sos",
#                                                                       "seed", "div_rank", "division") %>%
#     mutate(id = paste0(team, "_", season))%>%
#     dplyr::select(id, everything()),
#   calculate_standings(fast_scraper_schedules(2020)) %>% dplyr::select("season", "team","wins", "losses", "ties", "win_pct","sov", "sos",
#                                                                       "seed", "div_rank", "division") %>%
#     mutate(id = paste0(team, "_", season))%>%
#     dplyr::select(id, everything()),
#   calculate_standings(fast_scraper_schedules(2019)) %>% dplyr::select("season", "team","wins", "losses", "ties", "win_pct","sov", "sos",
#                                                                       "seed", "div_rank", "division") %>%
#     mutate(id = paste0(team, "_", season))%>%
#     dplyr::select(id, everything()),
#   calculate_standings(fast_scraper_schedules(2018)) %>% dplyr::select("season", "team","wins", "losses", "ties", "win_pct","sov", "sos",
#                                                                       "seed", "div_rank", "division") %>%
#     mutate(id = paste0(team, "_", season))%>%
#     dplyr::select(id, everything()),
#   calculate_standings(fast_scraper_schedules(2017)) %>% dplyr::select("season", "team","wins", "losses", "ties", "win_pct","sov", "sos",
#                                                                       "seed", "div_rank", "division") %>%
#     mutate(id = paste0(team, "_", season))%>%
#     dplyr::select(id, everything()),
#   calculate_standings(fast_scraper_schedules(2016)) %>% dplyr::select("season", "team","wins", "losses", "ties", "win_pct","sov", "sos",
#                                                                       "seed", "div_rank", "division") %>%
#     mutate(id = paste0(team, "_", season))%>%
#     dplyr::select(id, everything()),
#   calculate_standings(fast_scraper_schedules(2015)) %>% dplyr::select("season", "team","wins", "losses", "ties", "win_pct","sov", "sos",
#                                                                       "seed", "div_rank", "division") %>%
#     mutate(id = paste0(team, "_", season))%>%
#     dplyr::select(id, everything()),
#   calculate_standings(fast_scraper_schedules(2014)) %>% dplyr::select("season", "team","wins", "losses", "ties", "win_pct","sov", "sos",
#                                                                       "seed", "div_rank", "division") %>%
#     mutate(id = paste0(team, "_", season))%>%
#     dplyr::select(id, everything()),
#   calculate_standings(fast_scraper_schedules(2013)) %>% dplyr::select("season", "team","wins", "losses", "ties", "win_pct","sov", "sos",
#                                                                       "seed", "div_rank", "division") %>%
#     mutate(id = paste0(team, "_", season))%>%
#     dplyr::select(id, everything()),
#   calculate_standings(fast_scraper_schedules(2012)) %>% dplyr::select("season", "team","wins", "losses", "ties", "win_pct","sov", "sos",
#                                                                       "seed", "div_rank", "division") %>%
#     mutate(id = paste0(team, "_", season))%>%
#     dplyr::select(id, everything()),
#   calculate_standings(fast_scraper_schedules(2011)) %>% dplyr::select("season", "team","wins", "losses", "ties", "win_pct","sov", "sos",
#                                                                       "seed", "div_rank", "division") %>%
#     mutate(id = paste0(team, "_", season))%>%
#     dplyr::select(id, everything()),
#   calculate_standings(fast_scraper_schedules(2010)) %>% dplyr::select("season", "team","wins", "losses", "ties", "win_pct","sov", "sos",
#                                                                       "seed", "div_rank", "division") %>%
#     mutate(id = paste0(team, "_", season))%>%
#     dplyr::select(id, everything()),
#   calculate_standings(fast_scraper_schedules(2009)) %>% dplyr::select("season", "team","wins", "losses", "ties", "win_pct","sov", "sos",
#                                                                       "seed", "div_rank", "division") %>%
#     mutate(id = paste0(team, "_", season))%>%
#     dplyr::select(id, everything()),
#   calculate_standings(fast_scraper_schedules(2008)) %>% dplyr::select("season", "team","wins", "losses", "ties", "win_pct","sov", "sos",
#                                                                       "seed", "div_rank", "division") %>%
#     mutate(id = paste0(team, "_", season))%>%
#     dplyr::select(id, everything()),
#   calculate_standings(fast_scraper_schedules(2007)) %>% dplyr::select("season", "team","wins", "losses", "ties", "win_pct","sov", "sos",
#                                                                       "seed", "div_rank", "division") %>%
#     mutate(id = paste0(team, "_", season))%>%
#     dplyr::select(id, everything()),
#   calculate_standings(fast_scraper_schedules(2006)) %>% dplyr::select("season", "team","wins", "losses", "ties", "win_pct","sov", "sos",
#                                                                       "seed", "div_rank", "division") %>%
#     mutate(id = paste0(team, "_", season))%>%
#     dplyr::select(id, everything()),
#   calculate_standings(fast_scraper_schedules(2005)) %>% dplyr::select("season", "team","wins", "losses", "ties", "win_pct","sov", "sos",
#                                                                       "seed", "div_rank", "division") %>%
#     mutate(id = paste0(team, "_", season))%>%
#     dplyr::select(id, everything()),
#   calculate_standings(fast_scraper_schedules(2004)) %>% dplyr::select("season", "team","wins", "losses", "ties", "win_pct","sov", "sos",
#                                                                       "seed", "div_rank", "division") %>%
#     mutate(id = paste0(team, "_", season))%>%
#     dplyr::select(id, everything()),
#   calculate_standings(fast_scraper_schedules(2003)) %>% dplyr::select("season", "team","wins", "losses", "ties", "win_pct","sov", "sos",
#                                                                       "seed", "div_rank", "division") %>%
#     mutate(id = paste0(team, "_", season))%>%
#     dplyr::select(id, everything()),
#   calculate_standings(fast_scraper_schedules(2002)) %>% dplyr::select("season", "team","wins", "losses", "ties", "win_pct","sov", "sos",
#                                                                       "seed", "div_rank", "division") %>%
#     mutate(id = paste0(team, "_", season))%>%
#     dplyr::select(id, everything()),
#   calculate_standings(fast_scraper_schedules(2001)) %>% dplyr::select("season", "team","wins", "losses", "ties", "win_pct","sov", "sos",
#                                                                       "seed", "div_rank", "division") %>%
#     mutate(id = paste0(team, "_", season))%>%
#     dplyr::select(id, everything()),
#   calculate_standings(fast_scraper_schedules(2000)) %>% dplyr::select("season", "team","wins", "losses", "ties", "win_pct","sov", "sos",
#                                                                       "seed", "div_rank", "division") %>%
#     mutate(id = paste0(team, "_", season))%>%
#     dplyr::select(id, everything())
# )
# # 
# conversion_rates<-rbind(
#     calculate_series_conversion_rates(fast_scraper_schedules(2024) %>%
#                                         slice_tail(n = 32) %>%
#                                         fast_scraper(), weekly = FALSE) %>%
#       dplyr::select("season","team","off_scr_3rd", "def_scr_3rd") %>%
#       mutate(id = paste0(team, "_", season))%>%
#       dplyr::select(id, everything()),
#   calculate_series_conversion_rates(fast_scraper_schedules(2023) %>%
#                                       slice_tail(n = 32) %>%
#                                       fast_scraper(), weekly = FALSE) %>%
#     dplyr::select("season","team","off_scr_3rd", "def_scr_3rd") %>%
#     mutate(id = paste0(team, "_", season))%>%
#     dplyr::select(id, everything()),
#   calculate_series_conversion_rates(fast_scraper_schedules(2022) %>%
#                                       slice_tail(n = 32) %>%
#                                       fast_scraper(), weekly = FALSE) %>%
#     dplyr::select("season","team","off_scr_3rd", "def_scr_3rd") %>%
#     mutate(id = paste0(team, "_", season))%>%
#     dplyr::select(id, everything()),
#   calculate_series_conversion_rates(fast_scraper_schedules(2021) %>%
#                                       slice_tail(n = 32) %>%
#                                       fast_scraper(), weekly = FALSE) %>%
#     dplyr::select("season","team","off_scr_3rd",  "def_scr_3rd") %>%
#     mutate(id = paste0(team, "_", season))%>%
#     dplyr::select(id, everything()),
#   calculate_series_conversion_rates(fast_scraper_schedules(2020) %>%
#                                       slice_tail(n = 32) %>%
#                                       fast_scraper(), weekly = FALSE) %>%
#     dplyr::select("season","team","off_scr_3rd", "def_scr_3rd") %>%
#     mutate(id = paste0(team, "_", season))%>%
#     dplyr::select(id, everything()),
#   calculate_series_conversion_rates(fast_scraper_schedules(2019) %>%
#                                       slice_tail(n = 32) %>%
#                                       fast_scraper(), weekly = FALSE) %>%
#     dplyr::select("season","team","off_scr_3rd", "def_scr_3rd") %>%
#     mutate(id = paste0(team, "_", season))%>%
#     dplyr::select(id, everything()),
#   calculate_series_conversion_rates(fast_scraper_schedules(2018) %>%
#                                       slice_tail(n = 32) %>%
#                                       fast_scraper(), weekly = FALSE) %>%
#     dplyr::select("season","team","off_scr_3rd", "def_scr_3rd") %>%
#     mutate(id = paste0(team, "_", season))%>%
#     dplyr::select(id, everything()),
#   calculate_series_conversion_rates(fast_scraper_schedules(2017) %>%
#                                       slice_tail(n = 32) %>%
#                                       fast_scraper(), weekly = FALSE) %>%
#     dplyr::select("season","team","off_scr_3rd", "def_scr_3rd") %>%
#     mutate(id = paste0(team, "_", season))%>%
#     dplyr::select(id, everything()),
#   calculate_series_conversion_rates(fast_scraper_schedules(2016) %>%
#                                       slice_tail(n = 32) %>%
#                                       fast_scraper(), weekly = FALSE) %>%
#     dplyr::select("season","team","off_scr_3rd", "def_scr_3rd") %>%
#     mutate(id = paste0(team, "_", season))%>%
#     dplyr::select(id, everything()),
#   calculate_series_conversion_rates(fast_scraper_schedules(2015) %>%
#                                       slice_tail(n = 32) %>%
#                                       fast_scraper(), weekly = FALSE) %>%
#     dplyr::select("season","team","off_scr_3rd", "def_scr_3rd") %>%
#     mutate(id = paste0(team, "_", season))%>%
#     dplyr::select(id, everything()),
#   calculate_series_conversion_rates(fast_scraper_schedules(2014) %>%
#                                       slice_tail(n = 32) %>%
#                                       fast_scraper(), weekly = FALSE) %>%
#     dplyr::select("season","team","off_scr_3rd", "def_scr_3rd") %>%
#     mutate(id = paste0(team, "_", season))%>%
#     dplyr::select(id, everything()),
#   calculate_series_conversion_rates(fast_scraper_schedules(2013) %>%
#                                       slice_tail(n = 32) %>%
#                                       fast_scraper(), weekly = FALSE) %>%
#     dplyr::select("season","team","off_scr_3rd", "def_scr_3rd") %>%
#     mutate(id = paste0(team, "_", season))%>%
#     dplyr::select(id, everything()),
#   calculate_series_conversion_rates(fast_scraper_schedules(2012) %>%
#                                       slice_tail(n = 32) %>%
#                                       fast_scraper(), weekly = FALSE) %>%
#     dplyr::select("season","team","off_scr_3rd", "def_scr_3rd") %>%
#     mutate(id = paste0(team, "_", season))%>%
#     dplyr::select(id, everything()),
#   calculate_series_conversion_rates(fast_scraper_schedules(2011) %>%
#                                       slice_tail(n = 32) %>%
#                                       fast_scraper(), weekly = FALSE) %>%
#     dplyr::select("season","team","off_scr_3rd", "def_scr_3rd") %>%
#     mutate(id = paste0(team, "_", season))%>%
#     dplyr::select(id, everything()),
#   calculate_series_conversion_rates(fast_scraper_schedules(2010) %>%
#                                       slice_tail(n = 32) %>%
#                                       fast_scraper(), weekly = FALSE) %>%
#     dplyr::select("season","team","off_scr_3rd", "def_scr_3rd") %>%
#     mutate(id = paste0(team, "_", season))%>%
#     dplyr::select(id, everything()),
#   calculate_series_conversion_rates(fast_scraper_schedules(2009) %>%
#                                       slice_tail(n = 32) %>%
#                                       fast_scraper(), weekly = FALSE) %>%
#     dplyr::select("season","team","off_scr_3rd", "def_scr_3rd") %>%
#     mutate(id = paste0(team, "_", season))%>%
#     dplyr::select(id, everything()),
#   calculate_series_conversion_rates(fast_scraper_schedules(2008) %>%
#                                       slice_tail(n = 32) %>%
#                                       fast_scraper(), weekly = FALSE) %>%
#     dplyr::select("season","team","off_scr_3rd", "def_scr_3rd") %>%
#     mutate(id = paste0(team, "_", season))%>%
#     dplyr::select(id, everything()),
#   calculate_series_conversion_rates(fast_scraper_schedules(2007) %>%
#                                       slice_tail(n = 32) %>%
#                                       fast_scraper(), weekly = FALSE) %>%
#     dplyr::select("season","team","off_scr_3rd", "def_scr_3rd") %>%
#     mutate(id = paste0(team, "_", season))%>%
#     dplyr::select(id, everything()),
#   calculate_series_conversion_rates(fast_scraper_schedules(2006) %>%
#                                       slice_tail(n = 32) %>%
#                                       fast_scraper(), weekly = FALSE) %>%
#     dplyr::select("season","team","off_scr_3rd", "def_scr_3rd") %>%
#     mutate(id = paste0(team, "_", season))%>%
#     dplyr::select(id, everything()),
#   calculate_series_conversion_rates(fast_scraper_schedules(2005) %>%
#                                       slice_tail(n = 32) %>%
#                                       fast_scraper(), weekly = FALSE) %>%
#     dplyr::select("season","team","off_scr_3rd", "def_scr_3rd") %>%
#     mutate(id = paste0(team, "_", season))%>%
#     dplyr::select(id, everything()),
#   calculate_series_conversion_rates(fast_scraper_schedules(2004) %>%
#                                       slice_tail(n = 32) %>%
#                                       fast_scraper(), weekly = FALSE) %>%
#     dplyr::select("season","team","off_scr_3rd", "def_scr_3rd") %>%
#     mutate(id = paste0(team, "_", season))%>%
#     dplyr::select(id, everything()),
#   calculate_series_conversion_rates(fast_scraper_schedules(2003) %>%
#                                       slice_tail(n = 32) %>%
#                                       fast_scraper(), weekly = FALSE) %>%
#     dplyr::select("season","team","off_scr_3rd", "def_scr_3rd") %>%
#     mutate(id = paste0(team, "_", season))%>%
#     dplyr::select(id, everything()),
#   calculate_series_conversion_rates(fast_scraper_schedules(2002) %>%
#                                       slice_tail(n = 32) %>%
#                                       fast_scraper(), weekly = FALSE) %>%
#     dplyr::select("season","team","off_scr_3rd", "def_scr_3rd") %>%
#     mutate(id = paste0(team, "_", season))%>%
#     dplyr::select(id, everything()),
#   calculate_series_conversion_rates(fast_scraper_schedules(2001) %>%
#                                       slice_tail(n = 32) %>%
#                                       fast_scraper(), weekly = FALSE) %>%
#     dplyr::select("season","team","off_scr_3rd", "def_scr_3rd") %>%
#     mutate(id = paste0(team, "_", season))%>%
#     dplyr::select(id, everything()),
#   calculate_series_conversion_rates(fast_scraper_schedules(2000) %>%
#                                       slice_tail(n = 32) %>%
#                                       fast_scraper(), weekly = FALSE) %>%
#     dplyr::select("season","team","off_scr_3rd", "def_scr_3rd") %>%
#     mutate(id = paste0(team, "_", season))%>%
#     dplyr::select(id, everything())
# )
# 
# full.dat<-stats %>% left_join(standings, by = "id") %>% left_join(conversion_rates, by = "id") %>%
#   dplyr::select(-team.y, -season.y, -season.x, -team.x) %>%
#   dplyr::select(id, season, team, everything())
# 
# sb_history.dat<-data.frame(
#   id = c("KC_2023","KC_2022","LA_2021","TB_2020","KC_2019","NE_2018","PHI_2017","NE_2016","DEN_2015","NE_2014","SEA_2013","BAL_2012","NYG_2011","GB_2010","NO_2009","PIT_2008","NYG_2007","IND_2006","PIT_2005", "NE_2004", "NE_2003", "TB_2002", "NE_2001", "BAL_2000",
#          "SF_2023","PHI_2022","CIN_2021","KC_2020","SF_2019","LA_2018","NE_2017","ATL_2016","CAR_2015","SEA_2014","DEN_2013","SF_2012","NE_2011","PIT_2010","IND_2009","ARI_2008","NE_2007","CHI_2006","SEA_2005","PHI_2004","CAR_2003","OAK_2002","STL_2001","NYG_2000"
#   ),
#   Win_SB = c(rep(1, 24),rep(0, 24)),
#   Make_SB = rep(1, 48))
# 
# full.dat<-left_join(full.dat, sb_history.dat, by = "id")
# 
# full.dat$Win_SB<-ifelse(is.na(full.dat$Win_SB), 0 , full.dat$Win_SB)
# full.dat$Make_SB<-ifelse(is.na(full.dat$Make_SB), 0 , full.dat$Make_SB)
#  
# colSums(full.dat[, c(31,32)])
# 
# #writexl::write_xlsx(full.dat, "NFL team stats 2000-2024.xlsx")

full.dat<-openxlsx::read.xlsx("NFL team stats 2000-2024.xlsx")

## Adjust some data points
full.dat$fumbles_lost<-full.dat$rushing_fumbles_lost + full.dat$sack_fumbles_lost
full.dat$sack_fumbles_lost<-NULL
full.dat$rushing_fumbles_lost<-NULL

## Clean data of missingness
colnames(full.dat)
colSums(is.na(full.dat))
full.dat$passing_cpoe<-NULL
full.dat$seed<-ifelse(is.na(full.dat$seed), 0, full.dat$seed)
colnames(full.dat)
colSums(is.na(full.dat))

## Impute missing values for passing EPA and Rushing EPA
passing_epa_mod<-lm(passing_epa ~ penalty_yards + sack_yards_lost +
                      passing_interceptions + win_pct, data = full.dat[!is.na(full.dat$passing_epa),])
summary(passing_epa_mod)

(full.dat$passing_epa[is.na(full.dat$passing_epa)]<-predict(passing_epa_mod, newdata = full.dat[is.na(full.dat$passing_epa),]))

rushing_epa_mod<-lm(rushing_epa ~ passing_epa +  win_pct + fumbles_lost + sov, data = full.dat[!is.na(full.dat$rushing_epa),])
summary(rushing_epa_mod)

(full.dat$rushing_epa[is.na(full.dat$rushing_epa)]<-predict(rushing_epa_mod, newdata = full.dat[is.na(full.dat$rushing_epa),]))

rec_epa_mod<-lm(receiving_epa ~ passing_epa +  win_pct + fumbles_lost + sov, data = full.dat[!is.na(full.dat$receiving_epa),])
summary(rec_epa_mod)

(full.dat$receiving_epa[is.na(full.dat$receiving_epa)]<-predict(rec_epa_mod, newdata = full.dat[is.na(full.dat$receiving_epa),]))

off_scr_3rd_mod<-lm(off_scr_3rd ~ passing_epa + rushing_epa + receiving_epa + offensive_turnovers, data = full.dat[!is.na(full.dat$off_scr_3rd),])
summary(off_scr_3rd_mod)

(full.dat$off_scr_3rd[is.na(full.dat$off_scr_3rd)]<-predict(off_scr_3rd_mod, newdata = full.dat[is.na(full.dat$off_scr_3rd),]))

def_scr_3rd_mod<-lm(def_scr_3rd ~ win_pct + sov + sos, data = full.dat[!is.na(full.dat$def_scr_3rd),])
summary(def_scr_3rd_mod)

(full.dat$def_scr_3rd[is.na(full.dat$def_scr_3rd)]<-predict(def_scr_3rd_mod, newdata = full.dat[is.na(full.dat$def_scr_3rd),]))

colSums(is.na(full.dat))

full.dat<-na.omit(full.dat)

# Convert to Factor for stepwise variable selection for logistic regression
full.dat$Win_SB<-as.factor(full.dat$Win_SB)
full.dat$Make_SB<-as.factor(full.dat$Make_SB)
full.dat$seed<-as.factor(full.dat$seed)

# Adding Weights to correct for class imbalance
(prop<-table(full.dat$Make_SB)[1]/nrow(full.dat))
full.dat$w_Make_SB<-ifelse(full.dat$Make_SB == 1,9.5,.5)

# Split the data into training and test set
set.seed(40)
training.samples <- full.dat$Make_SB %>% 
  createDataPartition(p = 1, list = FALSE)

train.data  <- full.dat[training.samples, ]
test.data <- full.dat[-training.samples, ]

# Model
weighted_logit_model <- glm(Make_SB ~.,
                            family = binomial(link = "logit"), 
                            data = train.data%>%dplyr::select(-c("id", "team", "season", "games", 
                                                                 "div_rank", "division", "Win_SB","w_Make_SB", "seed", "ties", "losses", "wins")))%>%#,
                            #weights = train.data$w_Make_SB)%>%
                            stepAIC(trace = TRUE)

# Summarize the model
summary(weighted_logit_model)

# Make predictions
probabilities <- weighted_logit_model %>% predict(train.data, type = "response")

# Find Observation with highest probability in each Season and add a variable to indicate the observation
train.data$probabilities<-probabilities

#find roc curve
roc.curve <- pROC::roc(train.data$Make_SB, probabilities)
plot(roc.curve)
roc.curve$specificities
roc.curve$sensitivities
roc.curve$thresholds

# find elbow of ROC curve
pROC::coords(roc.curve, "best", ret = "threshold")
.0581/.5
# probabilities <- probabilities*8.606
train.data$probabilities<-probabilities
predicted.classes <- ifelse(probabilities > .05811, 1, 0)

# Model accuracy
mean(predicted.classes==train.data$Make_SB)
mean(probabilities)

# Confusion matrix
table(train.data$Make_SB, predicted.classes)

# Calculate Accuracy, Sensitivity and specificity
accuracy <- sum(predicted.classes == train.data$Make_SB) / length(train.data$Make_SB)
sensitivity <- sum(predicted.classes[train.data$Win_SB == 1] == 1) / sum(train.data$Win_SB == 1)
specificity <- sum(predicted.classes[train.data$Win_SB == 0] == 0) / sum(train.data$Win_SB == 0)

## Export the Data
writexl::write_xlsx(train.data, "NFL Super Bowl Modeling 2000-2024 v2.xlsx")
