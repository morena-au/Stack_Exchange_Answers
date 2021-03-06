library(cem)
library(dplyr)
library(survival)

#update.packages()

# Import file
setwd("C:/Projects/Stack_Exchange/01_motivation_feedback/Answers/data")
data_str_tr <- read.csv("data_str_tr_04_gt_ft.csv", stringsAsFactors = FALSE)
CEM_info <- read.csv("data_for_CEM.csv", stringsAsFactors = FALSE) 


# Adjust the variables
# Create tenure within the UX and tenure with SE (year when first join SE independently 
# from the community)

data_str_tr$TenureUX <- substring(data_str_tr$UX_registration, 1, 4)
data_str_tr$TenureUX <- as.numeric(data_str_tr$TenureUX)
data_str_tr$TenureSE <- substring(data_str_tr$SE_registration, 1, 4)
data_str_tr$TenureSE <- as.numeric(data_str_tr$TenureSE)

drop <- c("ParentId", "AccountId", "CreationDate", 
          "LastActivityDate", "LastAccessDate", 
          "UX_registration", "SE_registration", 
          "start_tenure", "tenure", "Autobiographer")


data_str_tr <- data_str_tr[ , !(names(data_str_tr) %in% drop)]

rm(drop)

# Renaming
colnames(data_str_tr)[which(names(data_str_tr) == "start_UX")] <- "StartUX"
colnames(data_str_tr)[which(names(data_str_tr) == "year")] <- "Year"
colnames(data_str_tr)[which(names(data_str_tr) == "weekday")] <- "WeekDay"


# Merge
data_str_tr <- merge(data_str_tr, CEM_info[, c("Id", "EditOwnAnswer", "AnswerOwnQuestion",
                                               "WordsCount", "externalSource_pct", "Mockups", 
                                               "CorrectGrammarScore", "TotRep" )], 
                     by = "Id", all.x = TRUE)

rm(CEM_info)

colnames(data_str_tr)[which(names(data_str_tr) == "externalSource_pct")] <- "ExternalSource_PCT"

# State dichotomous variables
data_str_tr$AcceptedByOriginator <- factor(data_str_tr$AcceptedByOriginator)
data_str_tr$StartUX <- factor(data_str_tr$StartUX)
data_str_tr$WeekDay <- factor(data_str_tr$WeekDay)
data_str_tr$EditOwnAnswer <- factor(data_str_tr$EditOwnAnswer)
data_str_tr$AnswerOwnQuestion <- factor(data_str_tr$AnswerOwnQuestion)

str(data_str_tr)
summary(data_str_tr) 

# There are some missing, CEM should be able to cope with them 
# (they mainly represent answers that have 
# been deleted at the time of extraction)


## 3.2.2 Coarsening by Explicit User Choice

# For the CATEGORICAL variables
# "AcceptedByOriginator", "WeekDay",  
# "EditOwnAnswer", "AnswerOwnQuestion", 
# "StartUX"

# leave them as they are (since they are dummy variables)


# For the NUMERICAL variables, we use cutpoints. Find meaningful natural breaks
# "EditCount", "UpMod", "DownMod", "CommentCount", 
#"Year",  "TenureUX", "TenureSE",
# "WordsCount", "ExternalSource_PCT", "Mockups",
# "CorrectGrammarScore",  "TotRep"

table(data_str_tr$Year)
yearCut <- c(2012, 2012.5, 2013.5, 2014.5, 2015.5, 2016.5, 2017.5, 2018)
data.frame(table(cut(data_str_tr$Year, breaks=yearCut, include.lowest = TRUE)))

table(data_str_tr$TenureSE)
SECut <- c(2008, 2008.5, 2009.5, 2010.5, 2011.5, 2012.5, 2013.5, 2014.5, 
           2015.5, 2016.5, 2017.5, 2018)
data.frame(table(cut(data_str_tr$TenureSE, breaks=SECut, include.lowest = TRUE)))

table(data_str_tr$TenureUX)
UXCut <- c(2010, 2010.5, 2011.5, 2012.5, 2013.5, 2014.5, 2015.5, 2016.5,
           2017.5, 2018.5, 2019)
data.frame(table(cut(data_str_tr$TenureUX, breaks=UXCut, include.lowest = TRUE)))

# Right tail distribution (Few bin in order to not lose observations)
table(data_str_tr$EditCount)
EditCut <- c(0, 0.5, 1.5, 2.5, max(data_str_tr$EditCount))
data.frame(table(cut(data_str_tr$EditCount, breaks=EditCut, include.lowest = TRUE)))


# Right tail distribution
table(data_str_tr$UpMod)
UpCut <- c(0, 0.5, 1.5, 2.5, 3.5, 5.5, 10.5, 20.5, 50.5, max(data_str_tr$UpMod))
data.frame(table(cut(data_str_tr$UpMod, breaks=UpCut, include.lowest = TRUE)))


# DownMod kind discrete variable with a right tail
table(data_str_tr$DownMod)
DownCut <- c(0, 0.5, 1.5, 2.5, max(data_str_tr$DownMod))
data.frame(table(cut(data_str_tr$DownMod, breaks=DownCut, include.lowest = TRUE)))


# CommentCount (max displayed are 5 in the stack exchange interface)
table(data_str_tr$CommentCount)
CommentCut <- c(0, 0.5, 1.5, 2.5, 3.5, 5.5, max(data_str_tr$CommentCount))
data.frame(table(cut(data_str_tr$CommentCount, breaks=CommentCut, include.lowest = TRUE)))

# WordsCount >> find a relevant bin variable
hist(data_str_tr$WordsCount)

fd_bin <- hist(data_str_tr$WordsCount, 
                     breaks = "FD")$breaks

Sturges_bin <- hist(data_str_tr$WordsCount, 
               breaks = "Sturges")$breaks

Scott_bin <- hist(data_str_tr$WordsCount, 
                    breaks = "Scott")$breaks

sqrt_bin <- hist(data_str_tr$WordsCount, 
                 breaks = sqrt(length(data_str_tr$WordsCount)))$breaks

data.frame(table(cut(data_str_tr$WordsCount, 
                     breaks=Scott_bin)))

# take the quintile distribution and makes some representative bins
# Lower and higher to be represented as well
summary(data_str_tr$WordsCount)
quintile_breaks <- c(seq(0, 55, length.out = 3), seq(55, 93, length.out = 3)[2:3],
  seq(93, 150, length.out = 3)[2:3], seq(150, 1800, length.out = 3)[2:3])

data.frame(table(cut(data_str_tr$WordsCount, 
                     breaks=quintile_breaks)))
hist(data_str_tr$WordsCount, 
     breaks = quintile_breaks)

WordsCut <- c(0.0, 27.5, 55.5, 74.5, 93.5, 121.5, 150.5, 975.5, 1800.0) 
data.frame(table(cut(data_str_tr$WordsCount, breaks=WordsCut, include.lowest = TRUE)))

# External Source Percentage
hist(data_str_tr$ExternalSource_PCT)

fd_bin <- hist(data_str_tr$ExternalSource_PCT, 
               breaks = "FD")$breaks

Sturges_bin <- hist(data_str_tr$ExternalSource_PCT, 
                    breaks = "Sturges")$breaks

Scott_bin <- hist(data_str_tr$ExternalSource_PCT, 
                  breaks = "Scott")$breaks

sqrt_bin <- hist(data_str_tr$ExternalSource_PCT, 
                 breaks = sqrt(length(data_str_tr$ExternalSource_PCT)))$breaks

data.frame(table(cut(data_str_tr$ExternalSource_PCT, 
                     breaks=Scott_bin)))

summary(data_str_tr$ExternalSource_PCT)
ExternalMax <- max(data_str_tr$ExternalSource_PCT[!is.na(data_str_tr$ExternalSource_PCT)])

ExternalCut <- c(0, 0.001, 5.5, 50.5, ExternalMax)
data.frame(table(cut(data_str_tr$ExternalSource_PCT, 
                     breaks=ExternalCut, include.lowest = TRUE)))


# "Mockups"
table(data_str_tr$Mockups)
summary(data_str_tr$Mockups)

MockupMax <- max(data_str_tr$Mockups[!is.na(data_str_tr$Mockups)])

MockCut <- c(0, 0.5, 1.5, 2.5, 4.5, MockupMax)
data.frame(table(cut(data_str_tr$Mockups, 
                     breaks=MockCut, include.lowest = TRUE)))


# "CorrectGrammarScore"
hist(data_str_tr$CorrectGrammarScore)
score_dist <- data.frame(table(data_str_tr$CorrectGrammarScore))
summary(data_str_tr$CorrectGrammarScore)

fd_bin <- hist(data_str_tr$CorrectGrammarScore, 
               breaks = "FD")$breaks

Sturges_bin <- hist(data_str_tr$CorrectGrammarScore, 
                    breaks = "Sturges")$breaks

Scott_bin <- hist(data_str_tr$CorrectGrammarScore, 
                  breaks = "Scott")$breaks

sqrt_bin <- hist(data_str_tr$CorrectGrammarScore, 
                 breaks = sqrt(length(data_str_tr$CorrectGrammarScore)))$breaks

data.frame(table(cut(data_str_tr$CorrectGrammarScore, 
                     breaks=Sturges_bin)))

GrammarCut <- c(75, 97.99, 98.99, 99.99, 100)

data.frame(table(cut(data_str_tr$CorrectGrammarScore, 
                     breaks=GrammarCut, include.lowest = TRUE)))



# "TotRep"
hist(data_str_tr$TotRep)
rep_dist <- data.frame(table(data_str_tr$TotRep))
summary(data_str_tr$TotRep)

fd_bin <- hist(data_str_tr$TotRep, 
               breaks = "FD")$breaks

Sturges_bin <- hist(data_str_tr$TotRep, 
                    breaks = "Sturges")$breaks

Scott_bin <- hist(data_str_tr$TotRep, 
                  breaks = "Scott")$breaks

sqrt_bin <- hist(data_str_tr$TotRep, 
                 breaks = sqrt(length(data_str_tr$TotRep)))$breaks

data.frame(table(cut(data_str_tr$TotRep, 
                     breaks=Scott_bin)))


# pick rep at 1, multple of 10s and at 100 (association bonus)
ReputationCut <- c(1, 1.5, 11.5, 100.5, 101.5, 151.5, 251.5, max(data_str_tr$TotRep))
data.frame(table(cut(data_str_tr$TotRep, 
                     breaks=ReputationCut, include.lowest = TRUE)))


# # validate summarize in a presentation
# Var1 Freq
# 1       (-1,1.5] 8175 > one vote, no contributions
# 2     (1.5,11.5] 1165 > first upvote (10), and other activities
# 3     (11.5,100] 3009 > Up to 9 upvotes (90), and other activities
# 4      (100,102] 1811 > association bonus (100), or 10 upvotes, or other activities
# 5      (102,152] 1698 > up to 5 upvotes (50) after the association bonus
# 6      (152,252] 1019 > up to 15 upvotes (150) after the association bonus
# 7 (252,2.36e+03]  794 > more than 15 upvotes after the association bonus or 25 upvotes, etc.

##### FIRST TREATMENT: AcceptedByOriginator #####
# Find the cutpoints for all the dataset
# then find the weights for each event 

# L1 statistics
vars <- c("EditCount", "UpMod", "DownMod", "CommentCount", 
          "Year", "StartUX", "WeekDay", "TenureUX", 
          "TenureSE", "EditOwnAnswer", "AnswerOwnQuestion",
          "WordsCount", "ExternalSource_PCT", "Mockups", 
          "CorrectGrammarScore", "TotRep")

imbalance(group=data_1$AcceptedByOriginator, data=data_1[vars])

# 3.2.1. Automated Coarsening

auto_matching <- cem(treatment = "AcceptedByOriginator", data = data_1, 
                      drop = c("Id", "OwnerUserId", "ParentId", 
                               "tstart", "tstop", "event",      
                               "status"), keep.all=TRUE)

# Number of observations matched (retained)
# Number of observation pruned because not comparable

auto_matching$w
auto_matching


# NUMERICAL
auto_matching$breaks$Year
auto_matching$breaks$TenureSE
auto_matching$breaks$TenureUX

auto_matching$breaks$WordsCount
auto_matching$breaks$ExternalSource_PCT
auto_matching$breaks$Mockups
auto_matching$breaks$CorrectGrammarScore
auto_matching$breaks$TotRep
auto_matching$breaks$EditCount
auto_matching$breaks$CommentCount



matching <- cem(treatment = "AcceptedByOriginator", data = data_1, 
                    drop = c("Id", "OwnerUserId", "ParentId", 
                            "tstart", "tstop", "event",      
                            "status"), 
                cutpoints = list(Year=yearcut, TenureSE=SEcut, TenureUX=UXcut,
                                 WordsCount=c("fd", na.rm = TRUE)))

data.frame(table(cut(data_1$WordsCount, breaks = matching$breaks$WordsCount)))

mat_controls
mat_controls$breaks$TenureSE

str(mat_controls)

mat_controls$group.idx  #index of observations belonging to each group
mat_controls$w #weights for use in the estimates of the causal effects


# take into account the other treatments variables and used them controls
mat_cont_treat <- cem(treatment = "AcceptedByOriginator", data = data_1, 
            drop = c("Id", "OwnerUserId", "ParentId", 
                     "tstart", "tstop", "event",                
                     "status"), 
            cutpoints = list(Year=yearcut, TenureSE=SEcut, TenureUX=UXcut))

mat_cont_treat


# Add one secs to tstop where tstart and tstop are equal (TT)
data_1$tstop <- ifelse(data_1$tstart == data_1$tstop, 
                       data_1$tstop + 1, data_1$tstop)

foo <- cbind(data_1, mat_controls$w)
colnames(foo)[25] <- "w"
foo <- subset(foo, w > 0)


model_pwp_tt = coxph(Surv(tstart, tstop, status) ~
                          AcceptedByOriginator +
                          Year +
                          StartUX +
                          WeekDay +
                          TenureUX +
                          TenureSE +
                          EditOwnAnswer +
                          AnswerOwnQuestion +
                          WordsCount +
                          ExternalSource_PCT +
                          Mockups +
                          CorrectGrammarScore +
                          TotRep +
                          cluster(OwnerUserId) + strata(event), method="breslow", 
                          data=foo, robust = TRUE, weights = w)



summary(model_pwp_tt)

# Treatments plus controls
bar <- cbind(data_1, mat_cont_treat$w)
colnames(bar)[25] <- "w"
bar <- subset(bar, w > 0)


# to do find the weight for all the strata from 1 to four and add them together
# prune observations where weight is zero

model_pwp_tt = coxph(Surv(tstart, tstop, status) ~
                       AcceptedByOriginator +
                       EditCount +
                       UpMod + 
                       DownMod +
                       CommentCount +
                       Year +
                       StartUX +
                       WeekDay +
                       TenureUX +
                       TenureSE +
                       EditOwnAnswer +
                       AnswerOwnQuestion +
                       WordsCount +
                       ExternalSource_PCT +
                       Mockups +
                       CorrectGrammarScore +
                       TotRep +
                       cluster(OwnerUserId) + strata(event), method="breslow", 
                       data=bar, robust = TRUE, weights = w)



summary(model_pwp_tt)


# reframe the other treatments as mutichotomous
# UpMod, DownMod, EditCount, CommentCount >> Coarsen treatment and apply CEM as usual (create intervals)
# https://gking.harvard.edu/files/gking/files/cem-washu.pdf
# 170
# Multicategory Treatments: No modification necessary; keep all strata
# with ≥ 1 unit having each value of T (L1 is max difference across
#                                       treatment groups)
# Continuous Treatments: Coarsen treatment and apply CEM as usual

# 3.6.1 Matching on Missingness
mat_cont_treat_miss <- cem("AcceptedByOriginator", data_str_tr_1, 
                           drop = c("Id", "OwnerUserId", "ParentId", 
                                    "tstart", "tstop", "event",                
                                    "status"),
                           cutpoints = list(Year=yearcut, TenureSE=SEcut, TenureUX=UXcut))

