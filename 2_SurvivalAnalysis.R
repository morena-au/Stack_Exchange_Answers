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


# Prentice, Williams and Peterson Gap Time 

data_str_tr_gt$AcceptedByOriginator <- factor(data_str_tr_gt$AcceptedByOriginator)
model_pwp_gt_00 = coxph(Surv(tstop-tstart,status) ~
                          UpMod +
                          DownMod +
                          CommentCount +
                          AcceptedByOriginator +
                          EditCount +
                          cluster(OwnerUserId) + strata(event), method="breslow", data=data_str_tr_gt, robust = TRUE)

summary(model_pwp_gt_00)


# Record the time dependent variable at the end of the interval - NO
data_str_all_gt <- read.csv("data_str_all_gt.csv", stringsAsFactors = FALSE)

data_str_all_gt_end <- data_str_all_gt %>%
  group_by(OwnerUserId) %>%
  arrange(CreationDate) %>%
  mutate(EditCount1 := lead(EditCount),       
         AcceptedByOriginator1 := lead(AcceptedByOriginator),
         UpMod1 := lead(UpMod),
         DownMod1 := lead(DownMod),
         CommentCount1 := lead(CommentCount))

data_str_all_gt_end[is.na(data_str_all_gt_end)] <- 0

# Truncate observations after the tenth event
data_str_all_gt_end <- subset(data_str_all_gt_end, event <= 4)

data_str_all_gt_end$AcceptedByOriginator1 <- factor(data_str_all_gt_end$AcceptedByOriginator1)
model_pwp_gt_00 = coxph(Surv(tstop-tstart,status) ~
                          UpMod1 +
                          DownMod1 +
                          CommentCount1 +
                          AcceptedByOriginator1 +
                          EditCount1 +
                          cluster(OwnerUserId) + strata(event), method="breslow", data=data_str_all_gt_end, robust = TRUE)

summary(model_pwp_gt_00)

# Change start and stop time in days
# result doesn't change if we calculate it in days for PWP - GT
data_str_tr_gt$tstart_day <- round(data_str_tr_gt$tstart/(24*60*60), 2)
data_str_tr_gt$tstop_day <- round(data_str_tr_gt$tstop/(24*60*60), 2)


# Prentice, Williams and Peterson Gap Time
model_pwp_gt_00 = coxph(Surv(tstop_day-tstart_day, status) ~
                          UpMod +
                          DownMod +
                          CommentCount +
                          AcceptedByOriginator +
                          EditCount +
                          cluster(OwnerUserId) + strata(event), method="breslow", data=data_str_tr_gt, robust = TRUE)

summary(model_pwp_gt_00)

# result doesn't change if we calculate it in days for PWP - TT
data_str_tr_tt$tstop_day <- round(data_str_tr_tt$tstop/(24*60*60), 2)
data_str_tr_tt$tstart_day <- round(data_str_tr_tt$tstart/(24*60*60), 2)

data_str_tr_tt$tstop_day <- ifelse(data_str_tr_tt$tstop_day == data_str_tr_tt$tstart_day, 
                                    data_str_tr_tt$tstop_day + 0.01, data_str_tr_tt$tstop_day)

model_pwp_tt_00 = coxph(Surv(tstart_day, tstop_day, status) ~
                          UpMod +
                          DownMod +
                          CommentCount +
                          AcceptedByOriginator +
                          EditCount +
                          cluster(OwnerUserId) + strata(event), method="breslow", data=data_str_tr_tt, robust = TRUE)

summary(model_pwp_tt_00)


