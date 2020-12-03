library(survival)

# Import file
setwd("C:/Projects/Stack_Exchange/motivation_feedback/Answers/data")
data_str_tr_tt <- read.csv("data_str_tr_tt.csv", stringsAsFactors = FALSE)
data_str_tr_gt <- read.csv("data_str_tr_gt.csv", stringsAsFactors = FALSE) 

# Prentice, Williams and Peterson  Total Time
model_pwp_tt_00 = coxph(Surv(tstart, tstop, status) ~
                          UpMod +
                          DownMod +
                          CommentCount +
                          AcceptedByOriginator +
                          EditCount +
                          cluster(OwnerUserId) + strata(event), method="breslow", data=data_str_tr_tt, robust = TRUE)

summary(model_pwp_tt_00)


# Prentice, Williams and Peterson Gap Time - WRONG assumption independence between the events
# in absolute value those that are not accepted answer (97%) have more chance to answer again

data_str_tr_gt$AcceptedByOriginator <- factor(data_str_tr_gt$AcceptedByOriginator)
model_pwp_gt_00 = coxph(Surv(tstop-tstart,status) ~
                          UpMod +
                          DownMod +
                          CommentCount +
                          AcceptedByOriginator +
                          EditCount +
                          cluster(OwnerUserId) + strata(event), method="breslow", data=data_str_tr_gt, robust = TRUE)

summary(model_pwp_gt_00)
