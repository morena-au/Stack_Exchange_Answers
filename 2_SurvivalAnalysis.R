library(survival)
library(coxme)

# Import file
setwd("C:/Projects/Stack_Exchange/motivation_feedback/Answers/data")
data_str_tr_tt <- read.csv("data_str_tr_tt.csv", stringsAsFactors = FALSE)
data_str_tr_gt <- read.csv("data_str_tr_gt.csv", stringsAsFactors = FALSE) 

data_str_tr_gt$year <- factor(data_str_tr_gt$year)
data_str_tr_gt$day <- factor(data_str_tr_gt$day)
data_str_tr_gt$start_UX <- factor(data_str_tr_gt$start_UX)
data_str_tr_gt$QuestionTag <- factor(data_str_tr_gt$QuestionTag)

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
# Make group comparisons by QuestionTag (ceteris paribus) > drop the answers with QuestionTags 
# where there are not treatment within-group comparisons.
# TODO interate multiple times until there are no more tags to delete
UpModTest <- as.data.frame.matrix(table(data_str_tr_gt$QuestionTag, data_str_tr_gt$UpMod))

# At least 2 answer within the tag with at least two different up voting levels (dealing with continuous variables)
UpModTest$count <- apply(UpModTest, 1, function(x) sum(x != 0))
UpModTest <- UpModTest["count"]

require(data.table)
UpModTest <- setDT(UpModTest, keep.rownames = TRUE)[]
setnames(UpModTest, 1, "QuestionTag")

TagsNoInfo <- UpModTest %>%
  filter(count == 1) %>%
  select(QuestionTag)

# Check for convergence
cens_tags <- as.data.frame.matrix(table(data_str_tr_gt$QuestionTag, data_str_tr_gt$status))
cens_tags_NoInfo <- subset(cens_tags, !(`0` != 0 & `1` != 0))
cens_tags_NoInfo <- setDT(cens_tags_NoInfo, keep.rownames = TRUE)[]
setnames(cens_tags_NoInfo, 1, "QuestionTag")

# Delete the users with at least one answer with a non informative tag
# Delete the answers with that question and the users
NoInfoUsers <- subset(data_str_tr_gt, QuestionTag %in% 
                        c(TagsNoInfo[['QuestionTag']], cens_tags_NoInfo[['QuestionTag']]))

data_str_tr_gt <- subset(data_str_tr_gt, !(OwnerUserId %in% unique(NoInfoUsers$OwnerUserId)))

data_str_tr_gt$QuestionTag <- factor(data_str_tr_gt$QuestionTag)



# # For dummy variables
# UpModTest$More_0 <- rowSums(UpModTest[, 2:dim(UpModTest)[2]])
# drops <- colnames(UpModTest)[3:dim(UpModTest)[2]-1]
# UpModTest <- UpModTest[ , !(names(UpModTest) %in% drops)]
# 
# UpModTest$QuestionTag <- row.names(UpModTest)
# 
# # remove tags with 0 in either 0 or more 0
# bar <- subset(UpModTest, `0` != 0 & More_0 != 0)

data_str_tr_gt$AcceptedByOriginator <- factor(data_str_tr_gt$AcceptedByOriginator)
model_pwp_gt_00 = coxph(Surv(tstop-tstart,status) ~
                          UpMod +
                          DownMod + # increase censuring especially in the 2nd, 3rd answer
                          cluster(OwnerUserId) + strata(event), method="breslow", data=data_str_tr_gt, robust = TRUE)

summary(model_pwp_gt_00)

# close interrelations of the start / end times with age may cause
# calculate it using the year the user first register (tenure)
model_pwp_gt_01 = coxph(Surv(tstop-tstart,status) ~
                          AcceptedByOriginator + # increase time to the next answer
                          EditCount +
                          UpMod +
                          DownMod + # increase censuring especially in the 2nd, 3rd answer
                          CommentCount + # increase censuring
                          tenure +
                          day +
                          start_UX +
                          QuestionTag +
                          cluster(OwnerUserId) + strata(event), method="breslow", data=data_str_tr_gt, robust = TRUE)

summary(model_pwp_gt_01)

model_pwp_gt_02 = coxph(Surv(tstop-tstart,status) ~
                          AcceptedByOriginator + # increase time to the next answer
                          EditCount +
                          UpMod +
                          DownMod + # increase censuring especially in the 2nd, 3rd answer
                          CommentCount + # increase censuring
                          cluster(OwnerUserId) + strata(event), method="breslow", data=data_str_tr_gt, robust = TRUE)

summary(model_pwp_gt_02)

