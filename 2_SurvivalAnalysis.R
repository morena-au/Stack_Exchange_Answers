library(survival)

# Import file
setwd("C:/Projects/Stack_Exchange/motivation_feedback/Answers/data")
data_str_tr <- read.csv("data_str_tr.csv", stringsAsFactors = FALSE)


model_pwp_tt_00 = coxph(Surv(tstart, tstop ,status) ~
                          UpMod +
                          DownMod +
                          CommentCount +
                          AcceptedByOriginator +
                          EditCount +
                          cluster(OwnerUserId) + strata(event), data=data_str_tr, robust = TRUE)

summary(model_pwp_tt_00)