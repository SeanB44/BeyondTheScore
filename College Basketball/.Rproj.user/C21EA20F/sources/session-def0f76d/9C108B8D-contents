source("../../Functions.R")
source("../../Codes.R")
usethis::edit_r_environ()
library(dplyr)
library(hoopR)
#login(user_email = Sys.getenv("KP_USER"), user_pw = Sys.getenv("KP_PW"))

## Update College Basketball ELO and Schedule Master ##
wb<-loadWorkbook("./2024-25 CBB Master.xlsx")

update_cbb_master(preseason_data,wb)

