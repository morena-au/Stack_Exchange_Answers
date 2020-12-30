library(dplyr)
library(survival)

# Import file
setwd("C:/Projects/Stack_Exchange/motivation_feedback/Answers/data")
data_str_tr_gt <- read.csv("data_str_tr_gt_top_tag_50.csv", stringsAsFactors = FALSE) 

# Adjust the variables
data_str_tr_gt$year <- factor(data_str_tr_gt$year) # year the answer was asked
data_str_tr_gt$start_UX <- factor(data_str_tr_gt$start_UX) # the contributor joined UX first
# data_str_tr_gt$tenure <- factor(data_str_tr_gt$tenure) # since when the contributor registered to one community in SE
data_str_tr_gt$TagCluster <- factor(data_str_tr_gt$TagCluster) # interests of the contributors (grouped by similar tag)
data_str_tr_gt$weekday <- factor(data_str_tr_gt$weekday) # contributor posted during weekdays or weekends 
data_str_tr_gt$Autobiographer <- factor(data_str_tr_gt$Autobiographer)
data_str_tr_gt$AcceptedByOriginator <- factor(data_str_tr_gt$AcceptedByOriginator)
data_str_tr_gt$score <- data_str_tr_gt$UpMod - data_str_tr_gt$DownMod

# gt model
model_pwp_gt_00 = coxph(Surv(tstop-tstart,status) ~
                          AcceptedByOriginator + 
                          EditCount +
                          # score +
                          UpMod +
                          DownMod +
                          CommentCount + 
                          start_UX +
                          weekday +
                          Autobiographer +
                          # year +
                          tenure +
                          # TagCluster +
                          cluster(OwnerUserId) + strata(event), method="breslow", data=data_str_tr_gt, robust = TRUE)

summary(model_pwp_gt_00)

