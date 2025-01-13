source("../../Functions.R")
usethis::edit_r_environ()

library(dplyr)
library(hoopR)

## Read in KenPom Data
login(user_email = Sys.getenv("KP_USER"), user_pw = Sys.getenv("KP_PW"))
kp.eff.dat<-kp_efficiency(min_year = 2022, max_year = 2025)

# ## BPI
# BPI.list<-list()
# for(i in c(2022:2024)){
# yr<-i
# url <- paste0("https://www.espn.com/mens-college-basketball/bpi/_/season/",yr)
# html_code <- read_html(url) # Read the HTML code of the page
# table_html <- html_code %>% html_nodes("table") # %>% .[[1]] # Use the html_nodes function to extract the table
# # Use the html_table function to convert the table
# BPI.dat.raw <- table_html %>% html_table() # HTML code into a data frame
# head(BPI.dat.raw)
# BPI.dat.raw<-cbind(BPI.dat.raw[[1]], BPI.dat.raw[[2]])
# colnames(BPI.dat.raw)<-BPI.dat.raw[1,]
# BPI.dat.raw<-BPI.dat.raw[-1,]
# BPI.list[[i-2020]]<-BPI.dat.raw
# names(BPI.list)[[i-2020]]<-i
# }

## Bart Torvik
BT.raw.dat<-read_xlsx("./2024-25 Preseason Projections.xlsx", sheet = 1)
miya_raw.dat<-read_xlsx("./2024-25 Preseason Projections.xlsx", sheet = 2)

## ensure team names are consistent
sort(BT.raw.dat$Team[which(!BT.raw.dat$Team %in% kp.eff.dat$team)])
sort(kp.eff.dat$team[which(!kp.eff.dat$team %in% BT.raw.dat$Team)])
sort(unique(kp.eff.dat$team))
kp.eff.dat$team<-case_when(kp.eff.dat$team == "Cal St. Northridge"~"CSUN",
                           kp.eff.dat$team == "IUPUI"~"IU Indy",
                           kp.eff.dat$team == "UMKC" ~ "Kansas City",
                           kp.eff.dat$team == "McNeese St." ~ "McNeese",
                           kp.eff.dat$team == "Nicholls St." ~ "Nicholls",
                           kp.eff.dat$team == "SIU Edwardsville" ~ "SIUE",
                           kp.eff.dat$team == "Southeast Missouri St." ~ "Southeast Missouri",
                           kp.eff.dat$team == "St. Francis PA" ~ "Saint Francis",
                           T ~ kp.eff.dat$team)

sort(BT.raw.dat$Team[which(!BT.raw.dat$Team %in% kp.eff.dat$team)])
sort(BT.raw.dat$Team[which(!kp.eff.dat$team %in% BT.raw.dat$Team)])
# sort(BT.raw.dat$Team)

BT.raw.dat$Team<-case_when(BT.raw.dat$Team == "IUPUI" ~ "IU Indy",
                           BT.raw.dat$Team == "Nicholls St." ~ "Nicholls",
                           BT.raw.dat$Team == "McNeese St." ~ "McNeese",
                           BT.raw.dat$Team == "UMKC" ~ "Kansas City",
                           BT.raw.dat$Team == "Cal St. Northridge" ~ "CSUN",
                           BT.raw.dat$Team == "Southeast Missouri St." ~ "Southeast Missouri",
                           BT.raw.dat$Team == "SIU Edwardsville" ~ "SIUE",
                           T ~ BT.raw.dat$Team)

sort(BT.raw.dat$Team[which(!BT.raw.dat$Team %in% kp.eff.dat$team)])
sort(kp.eff.dat$team[which(!kp.eff.dat$team %in% BT.raw.dat$Team)])
sort(BT.raw.dat$Team[which(!kp.eff.dat$team %in% BT.raw.dat$Team)])

# Evan Miya Names          
sort(miya_raw.dat$Team[which(!miya_raw.dat$Team %in% kp.eff.dat$team)])
# Replace State with St.
miya_raw.dat$Team<-gsub("State", "St.", miya_raw.dat$Team)
sort(miya_raw.dat$Team[which(!miya_raw.dat$Team %in% kp.eff.dat$team)])
sort(unique(kp.eff.dat$team))
miya_raw.dat$Team<-case_when(miya_raw.dat$Team == "Saint Francis (PA)" ~ "Saint Francis",
                             miya_raw.dat$Team == "Arkansas-Little Rock" ~ "Little Rock",
                             miya_raw.dat$Team == "Arkansas-Pine Bluff" ~ "Arkansas Pine Bluff",
                             miya_raw.dat$Team == "Bethune-Cookman" ~ "Bethune Cookman",
                             miya_raw.dat$Team == "California Baptist" ~ "Cal Baptist",
                             miya_raw.dat$Team == "College of Charleston" ~ "Charleston",
                             miya_raw.dat$Team == "Detroit" ~ "Detroit Mercy",
                             miya_raw.dat$Team == "Florida International" ~ "FIU",
                             miya_raw.dat$Team == "Fort Wayne" ~ "Purdue Fort Wayne",
                             miya_raw.dat$Team == "Gardner-Webb" ~ "Gardner Webb",
                             miya_raw.dat$Team == "Grambling" ~ "Grambling St.",
                             miya_raw.dat$Team == "Illinois-Chicago" ~ "Illinois Chicago",
                             miya_raw.dat$Team == "Long Island" ~ "LIU",
                             miya_raw.dat$Team == "Louisiana-Lafayette" ~ "Louisiana",
                             miya_raw.dat$Team == "Louisiana-Monroe" ~ "Louisiana Monroe",
                             miya_raw.dat$Team == "Loyola Maryland" ~ "Loyola MD",
                             miya_raw.dat$Team == "Maryland-Eastern Shore" ~ "Maryland Eastern Shore",
                             miya_raw.dat$Team == "Miami (Fla.)" ~ "Miami FL",
                             miya_raw.dat$Team == "Miami (Ohio)" ~ "Miami OH",
                             miya_raw.dat$Team == "Missouri-Kansas City" ~ "Kansas City",
                             miya_raw.dat$Team == "NC St." ~ "N.C. State",
                             miya_raw.dat$Team == "Ole Miss" ~ "Mississippi",
                             miya_raw.dat$Team == "Saint John's" ~ "St. John's",
                             miya_raw.dat$Team == "McNeese St." ~ "McNeese",
                             miya_raw.dat$Team == "Nicholls St." ~ "Nicholls",
                             miya_raw.dat$Team == "SIU Edwardsville" ~ "SIUE",
                             miya_raw.dat$Team == "Southeast Missouri St." ~ "Southeast Missouri",
                             miya_raw.dat$Team == "Prairie View" ~ "Prairie View A&M",
                             miya_raw.dat$Team == "Saint Bonaventure" ~ "St. Bonaventure",
                             miya_raw.dat$Team == "Saint Francis PA" ~ "Saint Francis",
                             miya_raw.dat$Team == "Omaha" ~ "Nebraska Omaha",
                             miya_raw.dat$Team == "Saint Thomas (Minn.)" ~ "St. Thomas",
                             miya_raw.dat$Team == "South Carolina Upstate" ~ "USC Upstate",
                             miya_raw.dat$Team == "Southern Mississippi" ~ "Southern Miss",
                             miya_raw.dat$Team == "Tennessee-Martin" ~ "Tennessee Martin",
                             miya_raw.dat$Team == "Texas-Rio Grande Valley" ~ "UT Rio Grande Valley",
                             miya_raw.dat$Team == "Texas A&M-Commerce" ~ "Texas A&M Commerce",
                             miya_raw.dat$Team == "Texas A&M-Corpus Christi" ~ "Texas A&M Corpus Chris",
                             miya_raw.dat$Team == "Cal St. Northridge" ~ "CSUN",
                             T ~ miya_raw.dat$Team)

sort(miya_raw.dat$Team[which(!miya_raw.dat$Team %in% kp.eff.dat$team)])
sort(miya_raw.dat$Team[which(!miya_raw.dat$Team %in% BT.raw.dat$Team)])          
sort(BT.raw.dat$Team[which(!BT.raw.dat$Team %in% kp.eff.dat$team)]) 

## Join Evan Miya, Bart Torvik and KenPom Data
full.dat<-full_join(kp.eff.dat%>%filter(year==2025), BT.raw.dat, by = c("team" = "Team"))%>%
  left_join(., miya_raw.dat, by = c("team" = "Team"))%>%
  dplyr::select("team", "conf", "adj_t", "adj_o", "adj_d", "AdjOE", "AdjDE", "Barthag","O-Rate", "D-Rate","Relative Rating")%>%
  rename("Team" = team,
         "Conference"=conf,
         "KenPom Adj T" = adj_t,
         "KenPom Adj O" = adj_o,
         "KenPom Adj D" = adj_d,
         "Torvik AdjOE" = AdjOE,
         "Torvik AdjDE" = AdjDE,
         "Torvik Barthag" = Barthag,
         "EvanMiya O-Rate" = `O-Rate`,
         "EvanMiya D-Rate" = `D-Rate`,
         "EvanMiya Relative Rating" = `Relative Rating`)%>%
  filter(Team!="Team")

full.dat<-full_join(full.dat, kp.eff.dat%>%filter(year == 2024)%>% dplyr::select("team", "adj_o", "adj_d"), by = c("Team" = "team"))%>%
  rename("KenPom Adj O 2024" = adj_o,
         "KenPom Adj D 2024" = adj_d)%>%
  mutate("KenPom AdjEM 2024" = `KenPom Adj O 2024` - `KenPom Adj D 2024`)%>%
  dplyr::select(-c("KenPom Adj O 2024", "KenPom Adj D 2024"))

full.dat$`Torvik AdjOE`<-as.numeric(full.dat$`Torvik AdjOE`)
full.dat$`Torvik AdjDE`<-as.numeric(full.dat$`Torvik AdjDE`)
full.dat$`Torvik Barthag`<-as.numeric(full.dat$`Torvik Barthag`)

full.dat$`KenPom AdjEM`<-full.dat$`KenPom Adj O` - full.dat$`KenPom Adj D`
full.dat$`Torvik AdjEM`<-full.dat$`Torvik AdjOE` - full.dat$`Torvik AdjDE`

## Calculate KenPom YoY Regreesion 
kp.eff.dat$AdjEM<-kp.eff.dat$adj_o - kp.eff.dat$adj_d
kp.eff.dat[order(kp.eff.dat$AdjEM, decreasing = T),c("team", "year", "AdjEM")]

kp.2022.dat<-kp.eff.dat%>%
  filter(year == 2022)%>%
  dplyr::select("team", "AdjEM")%>%
  rename("2022 AdjEM" = AdjEM)
kp.2023.dat<-kp.eff.dat%>%
  filter(year == 2023)%>%
  dplyr::select("team", "AdjEM")%>%
  rename("2023 AdjEM" = AdjEM)
kp.2024.dat<-kp.eff.dat%>%
  filter(year == 2024)%>%
  dplyr::select("team", "AdjEM")%>%
  rename("2024 AdjEM" = AdjEM)

kp.yoy.dat<-left_join(kp.2022.dat, kp.2023.dat, by = "team")%>%
  left_join(., kp.2024.dat, by = "team")%>%
  mutate("2022-2023 Change" = `2023 AdjEM` - `2022 AdjEM`,
         "2023-2024 Change" = `2024 AdjEM` - `2023 AdjEM`, 
         "2022 Average" = mean(`2022 AdjEM`, na.rm=T),
         "2023 Average" = mean(`2023 AdjEM`, na.rm=T),
         "2024 Average" = mean(`2024 AdjEM`, na.rm=T))

r<-mean(c(cor(kp.yoy.dat$`2022 AdjEM`, kp.yoy.dat$`2023 AdjEM`, use = "complete.obs"),
          cor(kp.yoy.dat$`2023 AdjEM`, kp.yoy.dat$`2024 AdjEM`, use = "complete.obs")))
p.rm<-100*(1-r)

## Add KenPom Regression to the mean to full data
full.dat$`KenPom AdjEM 2024`<-case_when(is.na(full.dat$`KenPom AdjEM 2024`) ~ full.dat$`KenPom AdjEM`,
                                        T ~ full.dat$`KenPom AdjEM 2024`)
full.dat$`KenPom AdjEM Regressed`<-full.dat$`KenPom AdjEM 2024`*r

## Calculate Bart Torvik Efficiency Metric
full.dat$`Torvik AdjEM`<-full.dat$`Torvik AdjOE` - full.dat$`Torvik AdjDE`

## Filter to get just necessary data
full.dat<-full.dat%>%
  dplyr::select("Team", "Conference", "KenPom AdjEM", "KenPom AdjEM Regressed", "Torvik AdjEM", "EvanMiya Relative Rating")%>%
  filter(!is.na(`KenPom AdjEM`))

# Ensure Normality
apply(full.dat[,-c(1:2)], 2, function(x) hist(x))

# Scale numeric values to z-score
full.dat$`KenPom AdjEM Regressed`<-case_when(is.na(full.dat$`KenPom AdjEM Regressed`) ~ full.dat$`KenPom AdjEM`,
                                             T ~ full.dat$`KenPom AdjEM`)
full.dat$`KenPom AdjEM`<-calculate_z_score(full.dat$`KenPom AdjEM`)
full.dat$`KenPom AdjEM Regressed`<-calculate_z_score(full.dat$`KenPom AdjEM Regressed`)
full.dat$`Torvik AdjEM`<-calculate_z_score(full.dat$`Torvik AdjEM`)
full.dat$`EvanMiya Relative Rating`<-calculate_z_score(full.dat$`EvanMiya Relative Rating`)

# Average Four ratings together to get aggregate
full.dat$`Aggregate Rating`<-rowMeans(full.dat[,-c(1:2)], na.rm = T)

# Scale to an ELO range
mean(full.dat$`Aggregate Rating`)

# Scale to a specific mean and standard deviation
sd<-sd(kp.eff.dat$adj_o - kp.eff.dat$adj_d)
u<-mean(kp.eff.dat$adj_o - kp.eff.dat$adj_d)

sd/max(kp.eff.dat$adj_o - kp.eff.dat$adj_d)
sd/min(kp.eff.dat$adj_o - kp.eff.dat$adj_d)
#SD is ~33% of the Max and Min

sd/(max(kp.eff.dat$adj_o - kp.eff.dat$adj_d) - min(kp.eff.dat$adj_o - kp.eff.dat$adj_d))
#SD is ~ 17% of the range

full.dat$`Pre Season ELO`<-((full.dat$`Aggregate Rating`*(1505*.115))+1505)

sd<-sd(full.dat$`Pre Season ELO`)
u<-mean(full.dat$`Pre Season ELO`)
sd/u
sd/max(full.dat$`Pre Season ELO`)
sd/min(full.dat$`Pre Season ELO`)         
max(full.dat$`Pre Season ELO`)/u
min(full.dat$`Pre Season ELO`)/u
(max(full.dat$`Pre Season ELO`) - min(full.dat$`Pre Season ELO`))/u
sd/(max(full.dat$`Pre Season ELO`) - min(full.dat$`Pre Season ELO`))
max(full.dat$`Pre Season ELO`)/(max(full.dat$`Pre Season ELO`) - min(full.dat$`Pre Season ELO`))
min(full.dat$`Pre Season ELO`)/(max(full.dat$`Pre Season ELO`) - min(full.dat$`Pre Season ELO`))


# ##Export all data
# wb<-createWorkbook()
# addWorksheet(wb, "Pre-Season 2024 Ratings")    
# addWorksheet(wb, "Conference Data")
# writeData(wb, "Pre-Season 2024 Ratings", PreSeason_2024_full)
# writeData(wb, "Conference Data", Conference_Avg_Elo)
# #saveWorkbook(wb, "./Output/Pre-Season College Football 2024 Ratings.xlsx", overwrite = TRUE)


## Read in schedule for 2024
schedule<-hoopR::load_mbb_schedule()
names(schedule)
schedule<-schedule%>% dplyr::select("id","date","season","season_type","start_date","home_id", "away_id",
                                    "venue_id", "home_location", "away_location", "venue_full_name",  
                                    "neutral_site", "conference_competition", "home_score", "away_score") %>%
  mutate("point_diff" = home_score - away_score,
         "result" = ifelse(point_diff>0, home_location, ifelse(point_diff<0, away_location, "")))
schedule  

## Read in team data
team.info<-ncaa_mbb_teams(year = 2024, division = 1)%>%
  dplyr::select(team_id, team_name, conference_id, conference)
team.info$team_id<-NULL

## Team ID's
team.ids<-schedule %>% dplyr::select("home_id", "home_location")%>%
  distinct() %>% 
  rename("team_id" = home_id) %>% 
  arrange(home_location)%>%
  rename("team" = home_location)

full.team.dat<-kp_hca()%>% dplyr::select(team, hca)%>%rename("Team" = team, "Home Court Advantage" = hca)%>%
  full_join(., full.dat, by = "Team")

full.team.dat$`Home Court Advantage`<-ifelse(is.na(full.team.dat$`Home Court Advantage`), mean(full.team.dat$`Home Court Advantage`, na.rm=T), full.team.dat$`Home Court Advantage`)


sort(team.ids$team[which(!team.ids$team %in% full.team.dat$Team)])

team.ids$kp_name<-team.ids$team
team.ids$kp_name<-gsub("State", "St.", team.ids$kp_name)
team.ids$kp_name<-gsub("-", " ", team.ids$kp_name)
team.ids$kp_name<-trimws(gsub("University", "", team.ids$kp_name))
team.ids$kp_name<-trimws(team.ids$kp_name)
sort(team.ids$kp_name[which(!team.ids$kp_name %in% full.team.dat$Team)])

team.ids$kp_name<-case_when(
  team.ids$kp_name == "App St." ~ "Appalachian St.",
  team.ids$kp_name == "Boston" ~ "Boston University",
  team.ids$kp_name == "Cal St. Northridge" ~ "CSUN",
  team.ids$kp_name == "California Baptist" ~ "Cal Baptist",
  team.ids$kp_name == "Florida International" ~ "FIU",
  team.ids$kp_name == "Grambling" ~ "Grambling St.",
  team.ids$kp_name == "Hawai'i" ~ "Hawaii",
  team.ids$kp_name == "IU Indianapolis" ~ "IU Indy",
  team.ids$kp_name == "Long Island" ~ "LIU",
  team.ids$kp_name == "Loyola Maryland" ~ "Loyola MD",
  team.ids$kp_name == "Miami" ~ "Miami FL",
  team.ids$kp_name == "Miami (OH)" ~ "Miami OH",
  team.ids$kp_name == "NC St." ~ "N.C. State",
  team.ids$kp_name == "Ole Miss" ~ "Mississippi",
  team.ids$kp_name == "Omaha" ~ "Nebraska Omaha",
  team.ids$kp_name == "Pennsylvania" ~ "Penn",
  team.ids$kp_name == "Sam Houston" ~ "Sam Houston St.",
  team.ids$kp_name == "Seattle U" ~ "Seattle",
  team.ids$kp_name == "San José St." ~ "San Jose St.",
  team.ids$kp_name == "SE Louisiana" ~ "Southeastern Louisiana",
  team.ids$kp_name == "SIU Edwardsville" ~ "SIUE",
  team.ids$kp_name == "St. Francis (PA)" ~ "Saint Francis",
  team.ids$kp_name == "South Carolina Upstate" ~ "USC Upstate",
  team.ids$kp_name == "Southeast Missouri St." ~ "Southeast Missouri",
  #team.ids$kp_name == "Southeast Louisiana" ~ "Southeastern Louisiana",
  team.ids$kp_name == "UAlbany" ~ "Albany",
  team.ids$kp_name == "UConn" ~ "Connecticut",
  team.ids$kp_name == "St. Thomas Minnesota" ~ "St. Thomas",
  #team.ids$kp_name == "St. Thomas (MN)" ~ "St. Thomas",
  team.ids$kp_name == "Texas A&M Corpus Christi" ~ "Texas A&M Corpus Chris",
  team.ids$kp_name == "UIC" ~ "Illinois Chicago",
  team.ids$kp_name == "UL Monroe" ~ "Louisiana Monroe",
  team.ids$kp_name == "UT Martin" ~ "Tennessee Martin",
  T ~ team.ids$kp_name)

# Confirm Naming
sort(team.ids$kp_name[which(!team.ids$kp_name %in% full.team.dat$Team)])
sort(full.team.dat$Team[which(!full.team.dat$Team %in% team.ids$kp_name)])

# sort(schedule$home_location[which(!schedule$home_location %in% team.ids$team)])
# sort(schedule$home_location[which(!schedule$home_location %in% full.team.dat$Team)])

full.team.dat$team_id<-team.ids$team_id[match(full.team.dat$Team, team.ids$kp_name)]

## Export to workbook

wb<-createWorkbook()
addWorksheet(wb, "Pre-Season 2024-25 Ratings")
writeData(wb, "Pre-Season 2024-25 Ratings", full.team.dat %>% dplyr::select("Team", "Conference", "Pre Season ELO") %>% arrange(-`Pre Season ELO`))
addWorksheet(wb, "Pre-Season Conference Ratings")
writeData(wb, "Pre-Season Conference Ratings", full.team.dat %>% group_by(Conference) %>% summarize("Average Pre Season ELO" = mean(`Pre Season ELO`)) %>% arrange(-`Average Pre Season ELO`))
addWorksheet(wb, "Conferences")
writeData(wb, "Conferences", full.team.dat %>% dplyr::select("Team", "Conference", "Pre Season ELO") %>% arrange(Conference, -`Pre Season ELO`))
addWorksheet(wb, "2025 Data")
writeData(wb, "2025 Data", left_join(full.team.dat[,c(1,2)],full.dat, by = "Team") %>% left_join(., kp.eff.dat %>% filter(year==2025), by = c("Team" = "team")))
addWorksheet(wb, "2024 Data")
writeData(wb, "2024 Data", left_join(full.team.dat[,c(1,2)],full.dat, by = "Team") %>% left_join(., kp.eff.dat %>% filter(year==2024), by = c("Team" = "team")))
#saveWorkbook(wb, "./Pre-Season 2024-25 Ratings FINAL.xlsx", overwrite = TRUE)

### Season Simulations ###

schedule$home_elo<-round(full.team.dat$`Pre Season ELO`[match(schedule$home_id, full.team.dat$team_id)],0)
schedule$away_elo<-round(full.team.dat$`Pre Season ELO`[match(schedule$away_id, full.team.dat$team_id)],0)
schedule$home_court_adv<-round(full.team.dat$`Home Court Advantage`[match(schedule$home_id, full.team.dat$team_id)], 2)

schedule$home_elo<-ifelse(is.na(schedule$home_elo), min(min(schedule$home_elo, na.rm = T), min(schedule$away_elo, na.rm=T)), schedule$home_elo)
schedule$away_elo<-ifelse(is.na(schedule$away_elo), min(min(schedule$home_elo, na.rm = T), min(schedule$away_elo, na.rm=T)), schedule$away_elo)

# Save Schedule to Excel
wb<-createWorkbook()
addWorksheet(wb, "2024-25 Schedule")
writeData(wb, "2024-25 Schedule", schedule)
saveWorkbook(wb, paste0("./2024-25 Schedule - ", Sys.Date(),".xlsx"), overwrite = TRUE)


## Monte Carlo Simulation
# 1) Simulate the season 10,000 times
# 2) Use the results to calculate the average number of wins for each team

## Function to simulate game
# Ha = Home win probability
# g = game number

sim_game<-function(Ha, g){
  if(runif(1)<Ha){
    return(c(schedule.dat.temp$home_location[g],
             schedule.dat.temp$Result[g]<-1))
    schedule.dat.temp$Winner[g]<schedule.dat.temp$home_location[g]
    schedule.dat.temp$Loser[g]<-schedule.dat.temp$away_location[g]
  }else{
    return(c(schedule.dat.temp$away_location[g],
             schedule.dat.temp$Result[g]<-0))
    schedule.dat.temp$Winner[g]<-schedule.dat.temp$away_team[g]
    schedule.dat.temp$Loser[g]<-schedule.dat.temp$home_team[g]
  }
}

schedule$Winner<-""
schedule$Loser<-""
schedule$Result<-NA
schedule$conf_game_winner<-""
schedule$conf_game_loser<-""
schedule$Conf_Result<-NA

# Initiate data frame to save simulated games
simulated.results<-data.frame("Team" = unique(schedule$home_location), "team_id" = unique(schedule$home_id))

# Set number of simulations
num.sims<-10000
#num.sims<-1 #Test
home_field_adv<-55
K<-32

schedule$home_elo<-ifelse(schedule$neutral_site, 0 + schedule$home_elo, home_field_adv+schedule$home_elo)

# Put schedule in order of games
schedule<-schedule[order(schedule$start_date, decreasing = F),]

# Ha = Home win prob
# Aa = Away win prob
# g = game
for(j in 1:num.sims){
  
  schedule.dat.temp<-schedule
  
  for(g in 1:nrow(schedule.dat.temp)){
    #g<-3000
    (Ha<-1/(1+10^(((schedule.dat.temp$away_elo[g]-schedule.dat.temp$home_elo[g]))/400))) #Calc Home win prob from Elo and add Home field advantage
    (Aa<-1/(1+10^((schedule.dat.temp$home_elo[g]-schedule.dat.temp$away_elo[g])/400))) #Calc away win prb from Elo and subtract home field advantage
    
    #Sim game and save result
    (simulated.game<-sim_game(Ha, g))
    (schedule.dat.temp$Result[g]<-as.numeric(simulated.game[2]))
    (schedule.dat.temp$Winner[g]<-simulated.game[1])
    (schedule.dat.temp$Loser[g]<-ifelse(schedule.dat.temp$Result[g]==1, schedule.dat.temp$away_location[g], schedule.dat.temp$home_location[g]))
    
    if(!schedule.dat.temp$conference_competition[g]){
      schedule.dat.temp$Conf_Result[g]<-NA
      schedule.dat.temp$conf_game_winner[g]<-NA
      schedule.dat.temp$conf_game_loser[g]<-NA
    }else{
      schedule.dat.temp$Conf_Result[g]<-schedule.dat.temp$Result[g]
      schedule.dat.temp$conf_game_winner[g]<-ifelse(schedule.dat.temp$Conf_Result[g]==1, schedule.dat.temp$home_location[g], schedule.dat.temp$away_location[g])
      schedule.dat.temp$conf_game_loser[g]<-ifelse( schedule.dat.temp$Conf_Result[g]==0, schedule.dat.temp$home_location[g], schedule.dat.temp$away_location[g])
    }
    ## Elo Shift from Loser to Winner - (K factor * Forecast Delta)
    #Adjust Elo for home team 
    (new.home.elo<-schedule.dat.temp$home_elo[g]+K*(ifelse(schedule.dat.temp$Result[g]==1, 1, 0)-Ha))
    (schedule.dat.temp$home_elo[which(schedule.dat.temp$home_location==schedule.dat.temp$home_location[g])]<-new.home.elo)
    (schedule.dat.temp$away_elo[which(schedule.dat.temp$away_location==schedule.dat.temp$home_location[g])]<-new.home.elo)
    
    #Adjust Elo for Away team
    (new.away.elo<-schedule.dat.temp$away_elo[g]+K*(ifelse(schedule.dat.temp$Result[g]==1, 0, 1)-Aa))
    (schedule.dat.temp$away_elo[which(schedule.dat.temp$away_location==schedule.dat.temp$away_location[g])]<-new.away.elo)
    (schedule.dat.temp$home_elo[which(schedule.dat.temp$home_location==schedule.dat.temp$away_location[g])]<-new.away.elo)
  }
  
  sim.dat<-data.frame(aggregate(Result~Winner, data=schedule.dat.temp, FUN=length))%>%rename("Team" = "Winner")
  sim.dat.conf<-data.frame(aggregate(Conf_Result~conf_game_winner, data=schedule.dat.temp, FUN=length))%>%rename("Team" = "conf_game_winner",  "Conference Result" = "Conf_Result")
  
  names(sim.dat)[2]<-paste0("Wins_",j)
  simulated.results<-left_join(simulated.results, sim.dat, by = "Team")
  names(sim.dat.conf)[2]<-paste0("Conference_Wins_",j)
  simulated.results<-left_join(simulated.results, sim.dat.conf, by = c("Team"))
  print(j)
}

simulated.results<-simulated.results[, -c(ncol(simulated.results)-1, ncol(simulated.results))]

# Replace all NA's in simulated results with 0
simulated.results[is.na(simulated.results)]<-0

## Calculate final Win Totals by Averaging together simulated seasons
num.sims<-(ncol(simulated.results)/2)-1
num.sims 

simulated.results[, c(1,ncol(simulated.results)-1, ncol(simulated.results))]

simulated.results$Projected_Wins<-rowMeans(simulated.results %>% dplyr::select(-matches("Conference_Wins_"), -matches("Team")), na.rm = T)
simulated.results$Projected_Conference_Wins<-rowMeans(simulated.results %>% dplyr::select(matches("Conference_Wins_"), -matches("Team")), na.rm = T)

simulated.results[order(simulated.results$Projected_Wins, decreasing=T),c(1, ncol(simulated.results)-1)]
simulated.results[order(simulated.results$Projected_Conference_Wins, decreasing=T),c(1, ncol(simulated.results))]

simulated.results$Projected_Wins<-round(simulated.results$Projected_Wins,0)
simulated.results$Projected_Conference_Wins<-round(simulated.results$Projected_Conference_Wins,0)

## Calculate Projected Losses
for(i in 1:length(unique(simulated.results$Team))){
  #i<-1
  simulated.results$Projected_Losses[i]<-sum(simulated.results$Team[i] == schedule$home_location,simulated.results$Team[i] == schedule$away_location ) - simulated.results$Projected_Wins[i]
  simulated.results$Projected_Conference_Losses[i]<-sum((simulated.results$Team[i] == schedule$home_location & schedule$conference_competition),simulated.results$Team[i] == schedule$away_location & schedule$conference_competition) - simulated.results$Projected_Conference_Wins[i]
}

## Calculate Win Percentage
simulated.results$Projected_WP<-simulated.results$Projected_Wins/(simulated.results$Projected_Wins + simulated.results$Projected_Losses)
simulated.results$Projected_WP<-round(simulated.results$Projected_WP,3)
simulated.results[order(simulated.results$Projected_WP, decreasing=T),c(1, ncol(simulated.results))]

simulated.results$Projected_Conference_WP<-simulated.results$Projected_Conference_Wins/(simulated.results$Projected_Conference_Wins + simulated.results$Projected_Conference_Losses)

simulations.dat<-simulated.results%>%
  dplyr::select("team_id","Team", "Projected_Wins", "Projected_Losses", "Projected_WP", "Projected_Conference_Wins", "Projected_Conference_Losses", "Projected_Conference_WP")%>%
  arrange(desc(Projected_WP))

simulations.dat<-left_join(simulated.results, full.team.dat%>% rename("tm" = Team), by = "team_id")%>%
  dplyr::select("team_id","Team", "Conference", "Projected_Wins", "Projected_Losses", "Projected_WP","Projected_Conference_Wins", "Projected_Conference_Losses", "Projected_Conference_WP")%>%
  arrange(Conference, desc(Projected_WP))

simulations.dat<-simulations.dat%>%
  group_by(Conference)%>%
  arrange(desc(Projected_Conference_WP))

## Add in final Elo for each team
Final_Elo.dat<-schedule.dat.temp[,c("home_location", "away_location", "home_elo", "away_elo")]%>%
  arrange(desc(home_elo))%>%
  distinct(home_location, home_elo)%>%
  rename("Team" = home_location, "Final Elo" = home_elo)

simulations.dat<-left_join(simulations.dat, Final_Elo.dat, by = "Team")

## Final Data
# Total
final.dat<-simulations.dat%>%
  dplyr::select("Team", "Conference", "Final Elo", "Projected_Wins", "Projected_Losses", "Win %" = "Projected_WP",
                "Projected_Conference_Wins", "Projected_Conference_Losses", "Conference Win%" = "Projected_Conference_WP")%>%
  mutate("Projected Record" = paste(Projected_Wins, "-", Projected_Losses, sep = ""))%>%
  dplyr::select(-c("Projected_Wins", "Projected_Losses"))%>%
  mutate("Projected Conference Record" = paste(Projected_Conference_Wins, "-", Projected_Conference_Losses, sep = ""))%>%
  dplyr::select(-c("Projected_Conference_Wins", "Projected_Conference_Losses"))%>%
  dplyr::select("Team", "Conference", "Final Elo", "Projected Record", "Win %", "Projected Conference Record", "Conference Win%")%>%
  arrange(desc(`Final Elo`))

# Conference
conference.final.dat<-final.dat %>%
  arrange(Conference, desc(`Conference Win%`))

## Export
wb<-createWorkbook()
addWorksheet(wb, "Final Projected Rankings")
addWorksheet(wb, "Final Conference Rankings")
writeData(wb,"Final Projected Rankings", final.dat)
writeData(wb,"Final Conference Rankings", conference.final.dat)
saveWorkbook(wb, "./Final 2024 Full-Season CBB Projections.xlsx", overwrite = F)

# ##### Simulate Conference Championship Games
# read.xlsx("./Output/Final 2024 Full-Season CFB Projections.xlsx", sheet = "Conference Championship Schedule")
# 
# for(j in 1:num.sims){
#   
#   schedule.dat.temp<-schedule.dat
#   
#   for(g in 1:nrow(schedule.dat.temp)){
#     #g<-1
#     (Ha<-1/(1+10^(((schedule.dat.temp$away_elo[g]-schedule.dat.temp$home_elo[g]))/400))) #Calc Home win prob from Elo and add Home field advantage
#     (Aa<-1/(1+10^((schedule.dat.temp$home_elo[g]-schedule.dat.temp$away_elo[g])/400))) #Calc away win prb from Elo and subtract home field advantage
#     
#     #Sim game and save result
#     (simulated.game<-sim_game(Ha, g))
#     (schedule.dat.temp$Result[g]<-as.numeric(simulated.game[2]))
#     (schedule.dat.temp$Winner[g]<-simulated.game[1])
#     (schedule.dat.temp$Loser[g]<-ifelse(schedule.dat.temp$Result[g]==1, schedule.dat.temp$away_team[g], schedule.dat.temp$home_team[g]))
#     
#     if(schedule.dat.temp$conf_game[g] != 1){
#       schedule.dat.temp$Conf_Result[g]<-NA
#       schedule.dat.temp$conf_game_winner[g]<-NA
#       schedule.dat.temp$conf_game_loser[g]<-NA
#     }else{
#       schedule.dat.temp$Conf_Result[g]<-schedule.dat.temp$Result[g]
#       schedule.dat.temp$conf_game_winner[g]<-ifelse(schedule.dat.temp$Conf_Result[g]==1, schedule.dat.temp$home_team[g], schedule.dat.temp$away_team[g])
#       schedule.dat.temp$conf_game_loser[g]<-ifelse( schedule.dat.temp$Conf_Result[g]==0, schedule.dat.temp$home_team[g], schedule.dat.temp$away_team[g])
#     }
#     ## Elo Shift from Loser to Winner - (K factor * Forecast Delta)
#     #Adjust Elo for home team 
#     schedule.dat.temp$home_elo[which(schedule.dat.temp$home_team==schedule.dat.temp$home_team[g])]<-schedule.dat.temp$home_elo[g]+K*(ifelse(schedule.dat.temp$Result[g]==1, 1, 0)-Ha)
#     schedule.dat.temp$away_elo[which(schedule.dat.temp$away_team==schedule.dat.temp$home_team[g])]<-schedule.dat.temp$home_elo[g]+K*(ifelse(schedule.dat.temp$Result[g]==1, 1, 0)-Ha)
#     
#     #Adjust Elo for Away team
#     schedule.dat.temp$away_elo[which(schedule.dat.temp$away_team==schedule.dat.temp$away_team[g])]<-schedule.dat.temp$away_elo[g]+K*(ifelse(schedule.dat.temp$Result[g]==1, 0, 1)-Aa)
#     schedule.dat.temp$home_elo[which(schedule.dat.temp$home_team==schedule.dat.temp$away_team[g])]<-schedule.dat.temp$away_elo[g]+K*(ifelse(schedule.dat.temp$Result[g]==1, 0, 1)-Aa)
#   }
#   
#   sim.dat<-data.frame(aggregate(Result~Winner, data=schedule.dat.temp, FUN=length))%>%rename("Team" = "Winner")
#   sim.dat.conf<-data.frame(aggregate(Conf_Result~conf_game_winner, data=schedule.dat.temp, FUN=length))%>%rename("Team" = "conf_game_winner",  "Conference Result" = "Conf_Result")
#   
#   names(sim.dat)[2]<-paste0("Wins_",j)
#   simulated.results<-left_join(simulated.results, sim.dat, by = "Team")
#   names(sim.dat.conf)[2]<-paste0("Conference_Wins_",j)
#   simulated.results<-left_join(simulated.results, sim.dat.conf, by = c("Team"))
#   print(j)
# }
# 
# 
# 
# # #### Create Model to Predict single game point differential based on Elo ####
# # 
# # games.2023<-cfbd_game_info(year = "2023") %>% dplyr::select(
# #   "season", "week", "home_team", "away_team", "home_pregame_elo", "away_pregame_elo", "home_points", "away_points") %>%
# #   mutate(elo_diff = home_pregame_elo - away_pregame_elo) %>%
# #   mutate(point_diff = home_points - away_points)
# # 
# # single.game.model<-lm(point_diff ~ elo_diff, data = games.2023)
# # summary(single.game.model)
# # 
# # ## make predictions for 2024
# # games.2022<-cfbd_game_info(year = "2022") %>% dplyr::select(
# #   "season", "week", "home_team", "away_team", "home_pregame_elo", "away_pregame_elo", "home_points", "away_points") %>%
# #   mutate(elo_diff = home_pregame_elo - away_pregame_elo) %>%
# #   mutate(point_diff = home_points - away_points)%>%
# #   filter(complete.cases(.))
# # 
# 
# # ## Model for just recruiting and Transfer Portal
# # ## Just since 2022 - When 247 began tracking
# # recruit_transfer.dat<-full.dat%>%
# #   filter(year %in% c(2022,2023))%>%
# #   dplyr::select("year", "team", "recruiting_score", "portal_points","returning_talent","return_ppa","prior_elo_rating","elo", "srs_rating")%>%
# #   na.omit()
# # 
# # ## Strictly Transfer Portal vs. Recruiting
# # set.seed(787)
# # recurit_transfer_linear.model1<-lm(elo ~ recruiting_score + portal_points, data = recruit_transfer.dat[(recruit_transfer.dat$year %in% c(2022, 2023)),-c(1:2)])
# # (model1_summary<-summary(recurit_transfer_linear.model1))
# # # Summary:
# #   # P-Value significant
# #   # R^2 = 0.405
# #   # Only Recruiting is significant
# # 
# # calc.relimp(recurit_transfer_linear.model1, rela = T)
# # relativeImportanceRecruitTransfer.out1<-data.frame("Variable" = names(calc.relimp(recurit_transfer_linear.model1, type = "lmg", rela = T)$lmg), "Relative Importance" = calc.relimp(recurit_transfer_linear.model1, type = "lmg", rela = T)$lmg, check.names = F)
# # # Sort by decreasing RI
# # relativeImportanceRecruitTransfer.out1<-relativeImportanceRecruitTransfer.out1[order(relativeImportanceRecruitTransfer.out1$`Relative Importance`, decreasing = T),]
# # relativeImportanceRecruitTransfer.out1
# # 
# # # Test significant difference
# # RecruitTransfer_StatTest<-boot.relimp(recurit_transfer_linear.model1, B = 1000)
# # ci<-booteval.relimp(RecruitTransfer_StatTest, norank=T)
# # ci #Significantly Different
# # 
# # ## Correlation Matrix
# # (corr.matrix<-cor(recruit_transfer.dat[,-c(1:2)]))
# # # Summary
# #   # SRS + Recruiting = 0.639
# #   # SRS + Transfer Portal = 0.393
# # 
# # ## Model adding in Returning talent & usage
# # recurit_transfer_linear.model2<-lm(elo ~ recruiting_score + portal_points + return_ppa, data = recruit_transfer.dat[(recruit_transfer.dat$year %in% c(2021,2022, 2023)),-c(1:2)])
# # (model2_summary<-summary(recurit_transfer_linear.model2))
# # # Summary:
# #   # P-Value significant
# #   # R^2 = 0.499
# #   # Portal and returning talent not significant
# # calc.relimp(recurit_transfer_linear.model2, rela = T)
# # relativeImportanceRecruitTransfer.out2<-data.frame("Variable" = names(calc.relimp(recurit_transfer_linear.model2, type = "lmg", rela = T)$lmg), "Relative Importance" = calc.relimp(recurit_transfer_linear.model2, type = "lmg", rela = T)$lmg, check.names = F)
# # # Sort by decreasing RI
# # relativeImportanceRecruitTransfer.out2<-relativeImportanceRecruitTransfer.out2[order(relativeImportanceRecruitTransfer.out2$`Relative Importance`, decreasing = T),]
# # relativeImportanceRecruitTransfer.out2
# # 
# # ## Recruiting Alone
# # recurit_transfer_linear.model3<-lm(elo ~ recruiting_score, data = recruit_transfer.dat[(recruit_transfer.dat$year %in% c(2022, 2023)),-c(1:2)])
# # (model3_summary<-summary(recurit_transfer_linear.model3))
# # # Summary:
# #   # P-Value significant
# #   # R^2 = 0.401
# #   # Recruiting is significant
# # 
# # ## Transfer Portal Alone
# # recurit_transfer_linear.model4<-lm(elo ~ portal_points, data = recruit_transfer.dat[(recruit_transfer.dat$year %in% c(2022, 2023)),-c(1:2)])
# # (model4_summary<-summary(recurit_transfer_linear.model4))
# # # Summary:
# #   # P-Value significant
# #   # R^2 = 0.151
# #   # Transfer Portal is significant
# # ## Export Results and data
# # 
# # ## Add in Additional Team Performance Data for 2024
# # additional_team_data.dat<-cfbd_rankings(year = "2023")%>%
# #   dplyr::filter(poll == "AP Top 25")%>%
# #   dplyr::filter(week == max(week))%>%
# #   dplyr::select("season","school", "rank", "conference")%>%
# #   rename(team = school, year = season)
# # 
# # additional_team_data.dat<-full_join(additional_team_data.dat, cfbd_game_records(year = "2023")%>%
# #   dplyr::select("year","team", "total_wins", "total_losses")%>%
# #   rename(Wins = total_wins, Losses = total_losses)%>%
# #   mutate(`Winning Percentage` = Wins/(Wins+Losses)), by = c("team","year"))
# # 
# # full.dat.new<-left_join(full.dat, additional_team_data.dat, by = c("team","year"))
# # 
# # wb<-createWorkbook()
# # addWorksheet(wb, "RI - Recruit+Transfer")
# # addWorksheet(wb, "RI - Recruit+Transfer+Return")
# # addWorksheet(wb, "Portal Data")
# # addWorksheet(wb, "Full Data")
# # writeData(wb, "RI - Recruit+Transfer", relativeImportanceRecruitTransfer.out1)
# # writeData(wb, "RI - Recruit+Transfer+Return", relativeImportanceRecruitTransfer.out2)
# # writeData(wb, "Portal Data", recruit_transfer.dat)
# # writeData(wb, "Full Data", full.dat.new)
# # ## Export
# # saveWorkbook(wb, "./Output/Recruit_Transfer_Portal_Regression.xlsx", overwrite = TRUE)
