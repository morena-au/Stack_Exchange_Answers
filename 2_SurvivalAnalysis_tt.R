library(dplyr)
library(survival)

# Import file
setwd("C:/Projects/Stack_Exchange/motivation_feedback/Answers/data")
data_str_tr_tt <- read.csv("data_str_tr_tt_top_tag_50.csv", stringsAsFactors = FALSE)

# Adjust the variables
data_str_tr_tt$year <- factor(data_str_tr_tt$year) # year the answer was asked
data_str_tr_tt$start_UX <- factor(data_str_tr_tt$start_UX) # the contributor joined UX first
data_str_tr_tt$tenure <- factor(data_str_tr_tt$tenure) # since when the contributor registered to one community in SE
data_str_tr_tt$TagCluster <- factor(data_str_tr_tt$TagCluster) # interests of the contributors (grouped by similar tag)
data_str_tr_tt$weekday <- factor(data_str_tr_tt$weekday) # contributor posted during weekdays or weekends

data_str_tr_tt$Autobiographer <- factor(data_str_tr_tt$Autobiographer)
data_str_tr_tt$score <- data_str_tr_tt$UpMod - data_str_tr_tt$DownMod

# tt Model
model_pwp_tt_00 = coxph(Surv(tstart, tstop, status) ~
                          AcceptedByOriginator + 
                          EditCount +
                          # score +
                          UpMod +
                          DownMod +
                          CommentCount + 
                          start_UX +
                          weekday +
                          Autobiographer +
                          year +
                          tenure +
                          TagCluster +
                          cluster(OwnerUserId) + strata(event), method="breslow", data=data_str_tr_tt, robust = TRUE)

summary(model_pwp_tt_00)

