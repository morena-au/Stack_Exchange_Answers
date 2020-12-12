library(survival)
library(coxme)

# Import file
setwd("C:/Projects/Stack_Exchange/motivation_feedback/Answers/data")
data_str_tr_tt <- read.csv("data_str_tr_tt.csv", stringsAsFactors = FALSE)
data_str_tr_gt <- read.csv("data_str_tr_gt.csv", stringsAsFactors = FALSE) 

# Prentice, Williams and Peterson  Total Time
model_pwp_tt_00 = coxph(Surv(tstart, tstop, status) ~
                          AcceptedByOriginator +
                          EditCount + # Reduce time between answers > general motivate to participate more
                          UpMod + # Reduce time between answers > general motivate to participate more
                          DownMod +
                          CommentCount + # Reduce time between answers > general motivate to participate more
                          cluster(OwnerUserId) + strata(event), method="breslow", data=data_str_tr_tt, robust = TRUE)

summary(model_pwp_tt_00)

# Prentice, Williams and Peterson  Total Time
model_pwp_tt_01 = coxph(Surv(tstart, tstop, status) ~
                          AcceptedByOriginator +
                          EditCount + # Reduce time between answers > general motivate to participate more
                          UpMod + # Reduce time between answers > general motivate to participate more
                          DownMod +
                          CommentCount + # Reduce time between answers > general motivate to participate more
                          strata(event), method="breslow", data=data_str_tr_tt)

summary(model_pwp_tt_01)

# Prentice, Williams and Peterson Gap Time 

data_str_tr_gt$AcceptedByOriginator <- factor(data_str_tr_gt$AcceptedByOriginator)
model_pwp_gt_00 = coxph(Surv(tstop-tstart,status) ~
                          AcceptedByOriginator + # increase time to the next answer
                          EditCount +
                          UpMod +
                          DownMod + # increase censuring especially in the 2nd, 3rd answer
                          CommentCount + # increase censuring
                          cluster(OwnerUserId) + strata(event), method="breslow", data=data_str_tr_gt, robust = TRUE)

summary(model_pwp_gt_00)

# Multilevel modeling of longitudinal data
# Mixed effects cox regression

'Subjects who are nested within the same higher level unit (question) are likely to have outcomes 
that are correlatetd with one. The within-cluster homogeneity may be induced by unmeasured cluster
characteristics.
- Cox models with random effects allows the baseline hazard function to vary across clusters
- use of random coefficients allows the effect of a given covariate to vary across clusters

which subject and question characteristics increase the likelihood of answering 
a question again after the first answer given..

(i) to what extent does the presence of positive feedback increase the likelihood of answering again?
(ii) does the magnitude of the effect of positive feedbacks on the hazard vary across question asked?

The data have a hierarchical structure, with answers nexted within questions.

Variation in the hazard and survival functions for a reference answer across questions.
A reference answer was a answer all of whose covariates were equal to zero.

A question with a relatively high likelihood of answer a question again will have relatively higher
survival funcion

Why should it be the case? what are those observable and unobservable charactersistics that make
a subjects who answered the same question more likely to answer again or viceversa?'

'Raykov - Day 5 page 31'
# https://stats.stackexchange.com/questions/228800/crossed-vs-nested-random-effects-how-do-they-differ-and-how-are-they-specified
# Prentice, Williams and Peterson  Total Time
model_pwp_tt_me = coxme(Surv(tstart, tstop, status) ~
                          AcceptedByOriginator +
                          EditCount + # Reduce time between answers > general motivate to participate more
                          UpMod + # Reduce time between answers > general motivate to participate more
                          DownMod +
                          CommentCount + # Reduce time between answers > general motivate to participate more
                          (1|ParentId) +
                          strata(event), data=data_str_tr_tt)

summary(model_pwp_tt_me)

model_pwp_tt_me1 = coxme(Surv(tstart, tstop, status) ~
                          AcceptedByOriginator +
                          EditCount + # Reduce time between answers > general motivate to participate more
                          UpMod + # Reduce time between answers > general motivate to participate more
                          DownMod +
                          CommentCount + # Reduce time between answers > general motivate to participate more
                          (1|OwnerUserId)+ 
                          strata(event), data=data_str_tr_tt)

summary(model_pwp_tt_me1)


model_pwp_gt_me = coxme(Surv(tstop-tstart,status) ~
                          AcceptedByOriginator + # increase time to the next answer
                          EditCount +
                          UpMod +
                          DownMod + # increase censuring especially in the 2nd, 3rd answer
                          CommentCount + # increase censuring
                          (1|ParentId) +
                          strata(event), data=data_str_tr_gt)

summary(model_pwp_gt_me)

# too many levels in order to consider crossed random effects
# control for observable question caracheristics >> (1|ParentId)+ (1|OwnerUserId)+
# running cox first and finding out which type of questions carrys 
# more active or censured

foo <- data_str_tr_tt %>%
  group_by(ParentId, OwnerUserId)%>%
  tally()
