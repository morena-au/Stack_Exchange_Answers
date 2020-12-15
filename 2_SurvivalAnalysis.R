library(survival)
library(coxme)

# Import file
setwd("C:/Projects/Stack_Exchange/motivation_feedback/Answers/data")
data_str_tr_tt <- read.csv("data_str_tr_tt.csv", stringsAsFactors = FALSE)
data_str_tr_gt <- read.csv("data_str_tr_gt.csv", stringsAsFactors = FALSE) 

data_str_tr_gt$year <- factor(data_str_tr_gt$year)
data_str_tr_gt$day <- factor(data_str_tr_gt$day)
data_str_tr_gt$start_UX <- factor(data_str_tr_gt$start_UX)

data_str_tr_tt$year <- factor(data_str_tr_tt$year)
data_str_tr_tt$day <- factor(data_str_tr_tt$day)
data_str_tr_tt$start_UX <- factor(data_str_tr_tt$start_UX)



# Prentice, Williams and Peterson  Total Time
model_pwp_tt_00 = coxph(Surv(tstart, tstop, status) ~
                          UpMod + # Reduce time between answers > general motivate to participate more
                          DownMod +
                          cluster(OwnerUserId) + strata(event), method="breslow", data=data_str_tr_tt, robust = TRUE)
summary(model_pwp_tt_00)

# Prentice, Williams and Peterson  Total Time
model_pwp_tt_01 = coxph(Surv(tstart, tstop, status) ~
                          AcceptedByOriginator +
                          UpMod + # Reduce time between answers > general motivate to participate more
                          DownMod +
                          cluster(OwnerUserId) + strata(event), method="breslow", data=data_str_tr_tt, robust = TRUE)

summary(model_pwp_tt_01)

# Prentice, Williams and Peterson  Total Time
model_pwp_tt_02 = coxph(Surv(tstart, tstop, status) ~
                          AcceptedByOriginator +
                          EditCount + # Reduce time between answers > general motivate to participate more
                          UpMod + # Reduce time between answers > general motivate to participate more
                          DownMod +
                          cluster(OwnerUserId) + strata(event), method="breslow", data=data_str_tr_tt, robust = TRUE)

summary(model_pwp_tt_02)

# Prentice, Williams and Peterson  Total Time
model_pwp_tt_03 = coxph(Surv(tstart, tstop, status) ~
                          AcceptedByOriginator +
                          EditCount + # Reduce time between answers > general motivate to participate more
                          UpMod + # Reduce time between answers > general motivate to participate more
                          DownMod +
                          CommentCount + # Reduce time between answers > general motivate to participate more
                          year +
                          day +
                          start_UX +
                          cluster(OwnerUserId) + strata(event), method="breslow", data=data_str_tr_tt, robust = TRUE)

summary(model_pwp_tt_03)


# Prentice, Williams and Peterson Gap Time 

data_str_tr_gt$AcceptedByOriginator <- factor(data_str_tr_gt$AcceptedByOriginator)
model_pwp_gt_00 = coxph(Surv(tstop-tstart,status) ~
                          UpMod +
                          DownMod + # increase censuring especially in the 2nd, 3rd answer
                          cluster(OwnerUserId) + strata(event), method="breslow", data=data_str_tr_gt, robust = TRUE)

summary(model_pwp_gt_00)

model_pwp_gt_01 = coxph(Surv(tstop-tstart,status) ~
                          AcceptedByOriginator + # increase time to the next answer
                          EditCount +
                          UpMod +
                          DownMod + # increase censuring especially in the 2nd, 3rd answer
                          CommentCount + # increase censuring
                          year +
                          day +
                          start_UX +
                          cluster(OwnerUserId) + strata(event), method="breslow", data=data_str_tr_gt, robust = TRUE)

summary(model_pwp_gt_01)
