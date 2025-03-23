# ## Libraries ##
library(cbbdata)
source("../../Codes.R")
source("../../Functions.R")
# login(user_email = Sys.getenv("KP_USER"), user_pw = Sys.getenv("KP_PW"))
cbbdata::cbd_login(username = Sys.getenv('CBD_USER'), password = Sys.getenv('CBD_PW'))

### NOTE - ONLY SET UP TO RUN AS FULL AT THIS TIME ####

sim.NCAAT<-function(team1 = NA, seed1=0, team2 = NA, seed2=0,
                    kp.data = "./Data/KP Full Table.xlsx",
                    bt.data = "./Data/Torvik Full Table.xlsx",
                    tourney.data = "./Data/2025 Seeding.xlsx",
                    run.all = TRUE){

## Source Logit and Linear Model for Predicting Outcomes and Point Totals
load("./Data/logit.model - NCAAT Probabilities.RData")
load("./Data/lm.model - NCAAT Point Totals.RData")

## Get Data of This Year
(torvikResume.dat<-cbbdata::cbd_torvik_current_resume())
(torvikMetrics.dat<-cbbdata::cbd_all_metrics(2025))
(torvikTRank<-cbbdata::cbd_torvik_resume_database(2025))
(kp.dat<-read.xlsx(kp.data))
(torvikTable.dat<-read.xlsx(bt.data))
(torvikTeamFactors.dat<-cbbdata::cbd_torvik_team_factors(2025))

logit.model$coefficients

# Get Variables and rename to be the same as coefficients (***Will be manual update if model is updated***)
kp.dat<-kp.dat[, -ncol(kp.dat)] %>% dplyr::rename(team = TeamName, KADJ.EM=AdjEM)
torvikResume.dat<-torvikResume.dat[-1,] %>% dplyr::rename(NET=net, ELO=elo, RESUME = resume, WAB=wab, WAB.RANK=wab_rk)
torvikTable.dat<-torvikTable.dat[,-ncol(torvikTable.dat)] %>% dplyr::rename(EFGPct=eFG, TOVPct="TOV%", TALENT=Talent)

# ## Ensure Team names are same (Need to re-check every year if team names change)
(kp.team.align<-which(!kp.dat$team %in% torvikResume.dat$team))
kp.dat$team[which(!kp.dat$team %in% torvikResume.dat$team)]

kp.dat$team<-case_when(kp.dat$team == "CSUN" ~ "Cal St. Northridge",
                       kp.dat$team == "Kansas City" ~ "UMKC",
                       kp.dat$team == "Nicholls" ~ "Nicholls St.",
                       kp.dat$team == "McNeese" ~ "McNeese St.",
                       kp.dat$team == "SIUE" ~ "SIU Edwardsville" ,
                       kp.dat$team == "Southeast Missouri" ~ "Southeast Missouri St.",
                       kp.dat$team == "East Texas A&M" ~ "Texas A&M Commerce", 
                       T~kp.dat$team)
# Confirm Cange & alignment
(kp.team.align<-which(!kp.dat$team %in% torvikResume.dat$team))                   
(kp.team.align<-which(!torvikResume.dat$team %in% kp.dat$team))   
(torvikTable.team.align<-which(!torvikTable.dat$Team %in% kp.dat$team))
(torvikTable.team.align<-which(!torvikTable.dat$Team %in% torvikResume.dat$team))

# Join all data (Will be manual update if variables change)
(full.current.dat<-left_join(kp.dat,
                       torvikResume.dat, by = "team") %>%
  left_join(torvikTable.dat, by = c("team" = "Team")))

full.current.dat<-full.current.dat%>%dplyr::select(-matches("..y"))
names(full.current.dat)<-gsub("\\.x", "", names(full.current.dat))

(current.dat<-left_join(kp.dat %>% dplyr::select(c(team, KADJ.EM)),
                       torvikResume.dat %>% dplyr::select(c(team,seed,conf,RESUME, WAB)), by = "team") %>%
  left_join(torvikTable.dat %>% dplyr::select(c(Team, TALENT)), by = c("team" = "Team")))

table(current.dat[1:68,"seed"], useNA="always")
current.dat %>% dplyr::filter(seed<=16)

tourney.dat<-read.xlsx(tourney.data)
names(tourney.dat)<-c("team", "SEED")
# Check tourney names
(team.align<-tourney.dat$team[which(!tourney.dat$team %in% current.dat$team)])

if(run.all){
  
  current.dat<-current.dat %>% filter(team %in% tourney.dat$team)
  current.dat<-current.dat %>% dplyr::select(c(team, conf, KADJ.EM, WAB, RESUME, TALENT))
  current.dat<-left_join(current.dat, tourney.dat, by = "team")
  names(current.dat)[ncol(current.dat)]<-"SEED"
  ncaat.model.data<-current.dat
# Create a matrix of all team combinations
team.combos<-expand.grid(current.dat$team, current.dat$team)
names(team.combos)<-c("FAVORITE", "UNDERDOG")
  
}else{
  team.combos<-data.frame(FAVORITE = team1, `FAVORITE SEED`=seed1, UNDERDOG = team2, `UNDERDOG SEED`= seed2, check.names = F)
  }

# Join data for FAVORITE from Current.dat
team.combos<-left_join(team.combos, current.dat, by = c("FAVORITE" = "team")) %>%
  rename("FAVORITE Conf" = conf, "FAVORITE KADJ.EM" = KADJ.EM, "FAVORITE WAB" = WAB, "FAVORITE TALENT" = TALENT, "FAVORITE RESUME" = RESUME, "FAVORITE SEED" = SEED)
# Join UNDERDOG data
team.combos<-left_join(team.combos, current.dat, by = c("UNDERDOG" = "team")) %>%
  rename("UNDERDOG Conf" = conf, "UNDERDOG KADJ.EM" = KADJ.EM, "UNDERDOG WAB" = WAB, "UNDERDOG TALENT" = TALENT, "UNDERDOG RESUME" = RESUME, "UNDERDOG SEED" = SEED)

# Create difference variables
team.combos$SEED.DIFFERENCE<-team.combos$`FAVORITE SEED` - team.combos$`UNDERDOG SEED`
team.combos$DIFFERENCE.KADJ.EM<-team.combos$`FAVORITE KADJ.EM` - team.combos$`UNDERDOG KADJ.EM`
team.combos$DIFFERENCE.WAB<-team.combos$`FAVORITE WAB` - team.combos$`UNDERDOG WAB`
team.combos$DIFFERENCE.RESUME<-team.combos$`FAVORITE RESUME` - team.combos$`UNDERDOG RESUME`
team.combos$DIFFERENCE.TALENT<-team.combos$`FAVORITE TALENT` - team.combos$`UNDERDOG TALENT`

## Run Logistic Regression on new data
logit.pred.full<-predict(logit.model, newdata = team.combos, type = "response")
logit.pred.outcome.full<-ifelse(logit.pred.full > 0.5, 1, 0)

# Create a dataframe of the predictions
team.combos$PREDICTION<-logit.pred.outcome.full
team.combos$PREDICTION.PROB<-logit.pred.full*100

# Calculate American Betting Odds from Win Probability
team.combos$BETTING.ODDS<-ifelse(team.combos$PREDICTION == 1, -1*(team.combos$PREDICTION.PROB/(1-(team.combos$PREDICTION.PROB/100))), (100/(team.combos$PREDICTION.PROB/100))-100)

# Predict Point Totals
team.combos$TOTAL.SCORE<-predict(lm.model, newdata = team.combos)

## Predict point spread
#formula: prob = 1 / (1 + 10^(-x / 400)) 
prob <- team.combos$PREDICTION.PROB / 100

log_fraction <- log10((1-prob)/prob)

x <- -400 * log_fraction

# Print result
team.combos$POINT.SPREAD<-abs(round(x, 1)/26.5)*ifelse(x > 0, -1, 1)

final.results<-team.combos %>% dplyr::select(c("FAVORITE", "UNDERDOG", "PREDICTION", "PREDICTION.PROB","POINT.SPREAD", "BETTING.ODDS", "TOTAL.SCORE"))
final.results$PREDICTION<-ifelse(final.results$PREDICTION == 1, final.results$`FAVORITE`, final.results$`UNDERDOG`)
final.results$PREDICTION.PROB<-round(final.results$PREDICTION.PROB, 1)

## Variable Importance for model
var.imp<-varImp(logit.model)
# scale to 100
var.imp$Overall<-var.imp$Overall/sum(var.imp$Overall)*100

out.results<-list("Results"=final.results, "Data"=full.current.dat, "Model Data"=team.combos)
return(out.results)
}

result.2025<-sim.NCAAT( kp.data = "./Data/KP Full Table.xlsx",
           bt.data = "./Data/Torvik Full Table.xlsx",
           tourney.data = "./Data/2025 Seeding.xlsx")
write.xlsx(result.2025, paste0("./Output/FULL NCAAT Simulations 2025 - ", Sys.Date(),".xlsx"), row.names = FALSE)
# wb<-createWorkbook()
# addWorksheet(wb, "NCAAT Simulations")
# writeData(wb, "NCAAT Simulations", result.2025)
# saveWorkbook(wb, file = "./Output/NCAAT Simulations v2.xlsx", overwrite = TRUE)
