## SURVIVAL ANALYSIS USEREXPERIENCE ANSWERS
library(plyr)
library(dplyr)
library(tidyr)
library(survival)
library(ggplot2)
library(lubridate)

# Import file
setwd("C:/Projects/Stack_Exchange/motivation_feedback/Answers/data")
d.ux.a.00 <- read.csv(file="./d.ux.a.00.csv", stringsAsFactors=FALSE)

keep <- c("ParentId", "Id", "CreationDate", "OwnerUserId",
          "LastEditorUserId", "LastEditDate", "LastActivityDate", 
          "Body.clean")

d.ux.a.01 <- d.ux.a.00[keep]

# Get User Info
Users <- read.csv(file="./QueryUsers.a.01.csv",stringsAsFactors=FALSE)

# Merge them together
d.ux.a.01 <- merge(d.ux.a.01, Users[, c("Id", "LastAccessDate", "AccountId")], 
                   by.x = "OwnerUserId", by.y = "Id", all.x = TRUE)
# 73 users have been deleted from SE

d.ux.a.01$LastAccessDate <- ifelse(is.na(d.ux.a.01$LastAccessDate), d.ux.a.01$LastActivityDate, 
                                   d.ux.a.01$LastAccessDate)

## Collect VOTES information
# Split UsersId since query reach the 50000 
# write.table(paste(UserId[1:755], collapse = ","),
#             "C:/Users/au517585/Desktop/Projects/Stack_Exchange/motivation_feedback/data/Answers/UserId.a.01A.txt",
#             sep = ",", row.names = FALSE, col.names = FALSE)
# 
# write.table(paste(UserId[756:1132], collapse = ","),
#             "C:/Users/au517585/Desktop/Projects/Stack_Exchange/motivation_feedback/data/Answers/UserId.a.01Ba.txt",
#             sep = ",", row.names = FALSE, col.names = FALSE)
# 
# write.table(paste(UserId[1133:1510], collapse = ","),
#             "C:/Users/au517585/Desktop/Projects/Stack_Exchange/motivation_feedback/data/Answers/UserId.a.01Bb.txt",
#             sep = ",", row.names = FALSE, col.names = FALSE)
# 
# write.table(paste(UserId[1511:2265], collapse = ","),
#             "C:/Users/au517585/Desktop/Projects/Stack_Exchange/motivation_feedback/data/Answers/UserId.a.01C.txt",
#             sep = ",", row.names = FALSE, col.names = FALSE)
# 
# write.table(paste(UserId[2266:3022], collapse = ","),
#             "C:/Users/au517585/Desktop/Projects/Stack_Exchange/motivation_feedback/data/Answers/UserId.a.01D.txt",
#             sep = ",", row.names = FALSE, col.names = FALSE)


# write.table(paste(UserId[3023:3778], collapse = ","),
#             "C:/Users/au517585/Desktop/Projects/Stack_Exchange/motivation_feedback/data/Answers/UserId.a.02A.txt",
#             sep = ",", row.names = FALSE, col.names = FALSE)
# 
# write.table(paste(UserId[3779:4534], collapse = ","),
#             "C:/Users/au517585/Desktop/Projects/Stack_Exchange/motivation_feedback/data/Answers/UserId.a.02B.txt",
#             sep = ",", row.names = FALSE, col.names = FALSE)
# 
# write.table(paste(UserId[4535:5290], collapse = ","),
#             "C:/Users/au517585/Desktop/Projects/Stack_Exchange/motivation_feedback/data/Answers/UserId.a.02C.txt",
#             sep = ",", row.names = FALSE, col.names = FALSE)
# 
# write.table(paste(UserId[5291:6044], collapse = ","),
#             "C:/Users/au517585/Desktop/Projects/Stack_Exchange/motivation_feedback/data/Answers/UserId.a.02D.txt",
#             sep = ",", row.names = FALSE, col.names = FALSE)

# write.table(paste(UserId[6045:7555], collapse = ","),
#             "C:/Users/au517585/Desktop/Projects/Stack_Exchange/motivation_feedback/data/Answers/UserId.a.03A.txt",
#             sep = ",", row.names = FALSE, col.names = FALSE)
# 
# write.table(paste(UserId[7556:9066], collapse = ","),
#             "C:/Users/au517585/Desktop/Projects/Stack_Exchange/motivation_feedback/data/Answers/UserId.a.03B.txt",
#             sep = ",", row.names = FALSE, col.names = FALSE)

# Import Votes by Post 
VotesPostDate_01_a <- read.csv(file="./QueryVotes_01A.csv",stringsAsFactors=FALSE)
VotesPostDate_01_ba <- read.csv(file="./QueryVotes_01Ba.csv",stringsAsFactors=FALSE)
VotesPostDate_01_bb <- read.csv(file="./QueryVotes_01Bb.csv",stringsAsFactors=FALSE)
VotesPostDate_01_c <- read.csv(file="./QueryVotes_01C.csv",stringsAsFactors=FALSE)
VotesPostDate_01_d <- read.csv(file="./QueryVotes_01D.csv",stringsAsFactors=FALSE)

VotesPostDate_02_a <- read.csv(file="./QueryVotes_02A.csv",stringsAsFactors=FALSE)
VotesPostDate_02_b <- read.csv(file="./QueryVotes_02B.csv",stringsAsFactors=FALSE)
VotesPostDate_02_c <- read.csv(file="./QueryVotes_02C.csv",stringsAsFactors=FALSE)
VotesPostDate_02_d <- read.csv(file="./QueryVotes_02D.csv",stringsAsFactors=FALSE)

VotesPostDate_03_a <- read.csv(file="./QueryVotes_03A.csv",stringsAsFactors=FALSE)
VotesPostDate_03_b <- read.csv(file="./QueryVotes_03B.csv",stringsAsFactors=FALSE)

VotesPostDate_04 <- read.csv(file="./QueryVotes_04.csv",stringsAsFactors=FALSE)

# Append user query
Votes <- rbind(VotesPostDate_01_a, VotesPostDate_01_ba, VotesPostDate_01_bb, 
               VotesPostDate_01_c, VotesPostDate_01_d, 
               VotesPostDate_02_a, VotesPostDate_02_b, VotesPostDate_02_c,
               VotesPostDate_02_d,
               VotesPostDate_03_a, VotesPostDate_03_b,
               VotesPostDate_04)

rm(VotesPostDate_01_a, VotesPostDate_01_ba, VotesPostDate_01_bb, 
  VotesPostDate_01_c, VotesPostDate_01_d, 
  VotesPostDate_02_a, VotesPostDate_02_b, VotesPostDate_02_c,
  VotesPostDate_02_d,
  VotesPostDate_03_a, VotesPostDate_03_b,
  VotesPostDate_04)

# Import Comments by Post and Date
CommentDate_01 <- read.csv(file="./QueryComments_01.csv",stringsAsFactors=FALSE)
CommentDate_02 <- read.csv(file="./QueryComments_02.csv",stringsAsFactors=FALSE)
CommentDate_03 <- read.csv(file="./QueryComments_03.csv",stringsAsFactors=FALSE)
CommentDate_04 <- read.csv(file="./QueryComments_04.csv",stringsAsFactors=FALSE)

# Append user query
Comments <- rbind(CommentDate_01, CommentDate_02, CommentDate_03, CommentDate_04)
rm(CommentDate_01, CommentDate_02, CommentDate_03, CommentDate_04)

d.ux.a.02 <- d.ux.a.01[d.ux.a.01$CreationDate < "2019-01-01 00:00:00", ]
# length(unique(d.ux.a.02$OwnerUserId))
# 
# user_freq <- count(d.ux.a.02, OwnerUserId)
# recurrence <- subset(user_freq, n >= 2)
# summary(recurrence)
# 
# rec3 <- subset(recurrence, n <= 3)
# rec4 <- subset(recurrence, n <= 4)
# rec5 <- subset(recurrence, n <= 5)
# rec6 <- subset(recurrence, n <= 6)
# rec7 <- subset(recurrence, n <= 7)
# rec8 <- subset(recurrence, n <= 8)
# rec9 <- subset(recurrence, n <= 9)
# rec10 <- subset(recurrence, n <= 10)
# summary(rec10)

## DATA STRUCTURE FOR MODELLING RECURRENT EVENT DATA
tmp <- d.ux.a.02 %>%
  group_by(OwnerUserId) %>%
  arrange(CreationDate) %>%
  mutate(tstart = as.numeric(difftime(CreationDate, lag(CreationDate), tz = "UTC"), units = "secs")) %>%
  mutate(tstop = as.numeric(difftime(lead(CreationDate), CreationDate, tz = "UTC"), units = "secs")) %>%
  mutate(event = row_number())

tmp$status <- ifelse(is.na(tmp$tstop), 0, 1)


# End date for censured observations
tmp$test <- ifelse(tmp$LastAccessDate <= "2019-01-01 00:00:00", # contributor lost before the end of the study
                   as.numeric(difftime(tmp$LastAccessDate, tmp$CreationDate, tz = "UTC"), units = "secs"),
                   as.numeric(difftime("2019-01-01 00:00:00", tmp$CreationDate, tz = "UTC"), units = "secs"))

# adjust users with LastAccessDate lower than CreationDate 
tmp$test <- ifelse(tmp$test < 0, 0, tmp$test)
tmp$tstop <- ifelse(is.na(tmp$tstop), tmp$test, tmp$tstop)
tmp$tstart[is.na(tmp$tstart)] = 0

# Add tstop and tstart all together they need to be on a continuous
tmp <- tmp %>%
  group_by(OwnerUserId) %>%
  arrange(CreationDate) %>%
  mutate(tstart = cumsum(tstart))

tmp <- tmp %>%
  group_by(OwnerUserId) %>%
  arrange(CreationDate) %>%
  mutate(tstop = cumsum(tstop))

tmp$test <- NULL

length(unique(tmp$OwnerUserId))
user_freq <- count(tmp, OwnerUserId)
table(user_freq$n)
user_freq <- subset(user_freq, n <= 10)
user_freq$OwnerUserId <- NULL
colnames(user_freq) <- "answer_seq"
contributors_seq <- count(user_freq, answer_seq)

ggplot(contributors_seq, aes(x = factor(answer_seq), y = n)) +
  geom_bar(stat = "identity", color="black", fill="white") +
  ggtitle("Count of contributors per total answers given") +
  ylab("Count") + 
  xlab("Total Answers") +
  theme_minimal()

# consider the full history 
data_str_all <- data.frame(tmp)

# Truncate observations after the tenth event
tmp <- subset(tmp, event <= 10)
cumulative_answers <- tmp[,c("event")]
cumulative_answers <- count(cumulative_answers, event)
data_str_a <- data.frame(tmp)
rm(tmp)

ggplot(cumulative_answers, aes(x = factor(event), y = n)) +
  geom_bar(stat = "identity", color="black", fill="white") +
  ggtitle("Count of contributors per sequence of the answer") +
  ylab("Count") + 
  xlab("Answer Sequence") +
  theme_minimal()



# setwd("C:/Users/au517585/Desktop/Projects/Stack_Exchange/motivation_feedback/data/Answers")
# write.csv(data_str_a, "data_str_a.csv")

'We consider data from a study with 11778 contributors in User Experience
Stack Exchange. The study is designed to evaluate the effect of covariates
on answering a question again.
All contributors entered the study once they answered the first question.
The event of interest is answer recurrence.
Contributors were censored at the time last visit to the website is
older than 6 months (https://stackoverflow.blog/2009/02/16/when-is-an-account-abandoned/).

Contributors were followed until 2018-12-31. 
40% of thecontributors had at least one recurrence (Answered at least 2 answers)
Median number of recurrences is 4, varying from 2 answers given to 919.
Among those with at least one recurrence, 79% had at most 10 recurrences.
We truncate the dataset after the tenth event due to the small number of events
in the later strata.'

length(unique(data_str_a$OwnerUserId))
user_freq <- count(data_str_a, OwnerUserId)
recurrence <- subset(user_freq, n >= 2)
summary(recurrence)

rec3 <- subset(recurrence, n <= 3)
rec4 <- subset(recurrence, n <= 4)
rec5 <- subset(recurrence, n <= 5)
rec6 <- subset(recurrence, n <= 6)
rec7 <- subset(recurrence, n <= 7)
rec8 <- subset(recurrence, n <= 8)
rec9 <- subset(recurrence, n <= 9)
rec10 <- subset(recurrence, n <= 10)
summary(rec10)


# Add one secs to tstop where tstart and tstop are equal (TT)
data_str_all$tstop <- ifelse(data_str_all$tstart == data_str_all$tstop, 
                           data_str_all$tstop + 1, data_str_all$tstop)

data_str_all$TimeBetweenAnswers <- data_str_all$tstop - data_str_all$tstart
hist(log(data_str_all$TimeBetweenAnswers))

# # Adjust for reverse causality
# # if edited before the next answer
# # Collect POST HISTORY information
# PostId <- unique(data_str_all$Id)
# 
# #Split PostId since query reach the 50000
# # > length(PostId)/3
# # [1] 23525
# write.table(paste(PostId[1:23525], collapse = ","),
#             "C:/Projects/Stack_Exchange/motivation_feedback/data/Answers/PostId_a1.txt",
#             sep = ",", row.names = FALSE, col.names = FALSE)
# 
# write.table(paste(PostId[23526:47051], collapse = ","),
#             "C:/Projects/Stack_Exchange/motivation_feedback/data/Answers/PostId_a2.txt",
#             sep = ",", row.names = FALSE, col.names = FALSE)
# 
# write.table(paste(PostId[47052:70575], collapse = ","),
#             "C:/Projects/Stack_Exchange/motivation_feedback/data/Answers/PostId_a3.txt",
#             sep = ",", row.names = FALSE, col.names = FALSE)

PostHistory_a1 <- read.csv(file="./PostHistory_a1.csv",stringsAsFactors=FALSE)
PostHistory_a2 <- read.csv(file="./PostHistory_a2.csv",stringsAsFactors=FALSE)
PostHistory_a3 <- read.csv(file="./PostHistory_a3.csv",stringsAsFactors=FALSE)

# Append user query
PostHistory <- rbind(PostHistory_a1, PostHistory_a2, PostHistory_a3)

rm(PostHistory_a1, PostHistory_a2, PostHistory_a3)

# data_str_a$EditDummy <- ifelse(is.na(data_str_a$LastEditDate), 0, 1)
# data_str_all$EditDummy <- ifelse(is.na(data_str_all$LastEditDate), 0, 1)

# # Change them into factors
# data_str_a$EditDummy <- factor(data_str_a$EditDummy)
# data_str_all$EditDummy <- factor(data_str_all$EditDummy)

# TODO: should not be relevant since the minority of the answers are edited
# # Include in the test whether last edit was made by the asking user (LastEditorUser = 1)
# data_str_q$LastEditorUser <- ifelse(data_str_q$OwnerUserId == data_str_q$LastEditorUserId, 1, 0)
# # input 0 to all the missing > the focus is given to the user that made the edit
# data_str_q$LastEditorUser[is.na(data_str_q$LastEditorUser)] <- 0
# data_str_q$LastEditorUser <- factor(data_str_q$LastEditorUser)

# Get the accumulative votes up, down until 3 months after the question
# was asked at a (constant time frame)

tmp_Votes <- data_str_a %>%
  group_by(OwnerUserId) %>%
  arrange(CreationDate) %>%
  select(c("OwnerUserId", "Id", "CreationDate"))


tmp_Votes <- merge(tmp_Votes, Votes[, c("PostId", "VoteTypeId", "VoteName",
                                        "VoteCreationDate")], by.x = "Id", by.y = "PostId")

# filter for the votes up to a certain date
tmp_Votes <- subset(tmp_Votes, VoteCreationDate <= ymd(as.Date(CreationDate)) %m+% months(3))

tmp_Votes <- tmp_Votes[, c("Id", "VoteName")]

tmp_Votes <- tmp_Votes %>%
  group_by(Id) %>%
  summarise(UpMod = sum(VoteName == "UpMod"),
            DownMod = sum(VoteName == "DownMod"))

data_str_a <- merge(data_str_a, tmp_Votes, by = "Id", all.x = TRUE)
rm(tmp_Votes)

#votes for all
tmp_Votes <- data_str_all %>%
  group_by(OwnerUserId) %>%
  arrange(CreationDate) %>%
  select(c("OwnerUserId", "Id", "CreationDate"))


tmp_Votes <- merge(tmp_Votes, Votes[, c("PostId", "VoteTypeId", "VoteName",
                                        "VoteCreationDate")], by.x = "Id", by.y = "PostId")

# filter for the votes up to a certain date
tmp_Votes <- subset(tmp_Votes, VoteCreationDate <= ymd(as.Date(CreationDate)) %m+% months(3))

tmp_Votes <- tmp_Votes[, c("Id", "VoteName")]

tmp_Votes <- tmp_Votes %>%
  group_by(Id) %>%
  summarise(UpMod = sum(VoteName == "UpMod"),
            DownMod = sum(VoteName == "DownMod"))

data_str_all <- merge(data_str_all, tmp_Votes, by = "Id", all.x = TRUE)
rm(tmp_Votes)

# Cope with missing regarding vote (questions in the same day, no vote at all)
data_str_all$UpMod[is.na(data_str_all$UpMod)] <- 0
data_str_all$DownMod[is.na(data_str_all$DownMod)] <- 0
data_str_all$score <- data_str_all$UpMod - data_str_all$DownMod
data_str_all$up_votes_ratio <- (data_str_all$UpMod)/(data_str_all$UpMod + data_str_all$DownMod)
data_str_all$up_votes_ratio <- ifelse(is.na(data_str_all$up_votes_ratio), 0, 
                                       data_str_all$up_votes_ratio)
# INTERPRETATION
# 0 >> NO VOTE
# 0 >> ALL NEGATIVE
# 0 < X < 1 >> FRACTION OF POSITIVE 
# 1 >> ALL POSITIVE

# data_str_a$fraction_down_score <- (data_str_a$DownMod)/(data_str_a$UpMod + data_str_a$DownMod)
# data_str_a$fraction_down_score <- ifelse(is.na(data_str_a$fraction_down_score), 0, 
#                                        data_str_a$fraction_down_score)
# 
# p <- ggplot(data_str_a, aes(x = status, y = score, group = status, fill = status)) + # group = Close,
#   geom_violin(trim=FALSE)
# 
# p

# Get accumulative comments count after a constant time frame since answer publication
tmp_Comments <- data_str_a %>%
  group_by(OwnerUserId) %>%
  arrange(CreationDate) %>%
  select(c("OwnerUserId", "Id", "CreationDate"))

tmp_Comments <- merge(tmp_Comments, Comments[, c("PostId", "CommentCreationDate")], 
                      by.x = "Id", by.y = "PostId")

# filter for the comments up to a certain date
tmp_Comments <- subset(tmp_Comments, CommentCreationDate <= ymd(as.Date(CreationDate)) %m+% months(3))

tmp_Comments <- tmp_Comments %>%
  group_by(Id) %>%
  tally()

colnames(tmp_Comments)[2] <- "CommentCount"
data_str_a <- merge(data_str_a, tmp_Comments, by = "Id", all.x = TRUE)
rm(tmp_Comments)

data_str_a$CommentCount[is.na(data_str_a$CommentCount)] <- 0
data_str_a$CommentDummy <- ifelse(data_str_a$CommentCount == 0, 0, 1)
data_str_a$CommentDummy <- factor(data_str_a$CommentDummy)


# Comments for TT
tmp_Comments <- data_str_all %>%
  group_by(OwnerUserId) %>%
  arrange(CreationDate) %>%
  select(c("OwnerUserId", "Id", "CreationDate"))

tmp_Comments <- merge(tmp_Comments, Comments[, c("PostId", "CommentCreationDate")], 
                      by.x = "Id", by.y = "PostId")

# filter for the comments up to a certain date
tmp_Comments <- subset(tmp_Comments, CommentCreationDate <= ymd(as.Date(CreationDate)) %m+% months(3))

tmp_Comments <- tmp_Comments %>%
  group_by(Id) %>%
  tally()

colnames(tmp_Comments)[2] <- "CommentCount"
data_str_all <- merge(data_str_all, tmp_Comments, by = "Id", all.x = TRUE)
rm(tmp_Comments)

data_str_all$CommentCount[is.na(data_str_all$CommentCount)] <- 0
data_str_all$CommentDummy <- ifelse(data_str_all$CommentCount == 0, 0, 1)
data_str_all$CommentDummy <- factor(data_str_all$CommentDummy)

# TODO consider making it a dummy

data_str_a$year <- substring(data_str_a$CreationDate, 1, 4)
data_str_a$year <- factor(data_str_a$year)

# For ALL
data_str_all$year <- substring(data_str_all$CreationDate, 1, 4)
data_str_all$year <- factor(data_str_all$year)

# # YEAR descriptive
# ## % of censured per year start to increase in the year 2014
# table(data_str_a$year, data_str_a$status)
# 
# cens_year <- as.data.frame.matrix(table(data_str_a$year, data_str_a$status))
# cens_year$cens_pct <- cens_year$`0`/(cens_year$`0` + cens_year$`1`)*100
# cens_year$event_pct <- cens_year$`1`/(cens_year$`0` + cens_year$`1`)*100
# cens_year$total_comment <- cens_year$`0` + cens_year$`1`
# 
# cens <- data.frame(cens_year$cens_pct)
# active <- data.frame(cens_year$event_pct)
# 
# names(cens) <- "obs"
# names(active) <- "obs"
# cens$year <- c(2008:2018)
# active$year <- c(2008:2018)
# cens$status <- 0
# active$status <- 1
# 
# status_year <- rbind(cens, active)
# status_year$status <- factor(status_year$status)
# # Overlaid histograms
# 
# ggplot(status_year, aes(x=year, y=obs, fill = status)) + 
#   geom_bar(stat = "identity", position="dodge", alpha = 0.7) +
#   labs(title="UX answers pct activiy across years \nStatus = 0 > Censured - Status = 1 > Active")

# Get participants who answer after the official launch
year_tmp <- data_str_a[data_str_a$CreationDate < "2012-01-03 21:00:00", ]
data_str_a <- subset(data_str_a, !(OwnerUserId %in% year_tmp$OwnerUserId))
data_str_a$year <- factor(data_str_a$year)

# For all 
year_tmp <- data_str_all[data_str_all$CreationDate < "2012-01-03 21:00:00", ]
data_str_all <- subset(data_str_all, !(OwnerUserId %in% year_tmp$OwnerUserId))
data_str_all$year <- factor(data_str_all$year)



# # Remove unregistered participants
# # Construct api call for each users
# 
# users_urls_api <- data.frame(unique(data_str_a$OwnerUserId))
# names(users_urls_api) <- "url"
# 
# users_urls_api$url <- paste("https://api.stackexchange.com/2.2/users/", users_urls_api$url,
#                           "?site=ux.stackexchange&key=G0yd6IHl5kBtkBtsNU*4dg((", sep = "")
# 
# setwd("C:/Users/au517585/Desktop/Projects/Stack_Exchange/motivation_feedback/data/Answers")
# write.csv(users_urls_api, "./users_urls_api.csv", row.names = FALSE)
# 
# # Subset for the missing calls
# users_info <- read.csv(file="./users_info.csv", stringsAsFactors=FALSE)
# users_urls_api <- read.csv(file="./users_urls_api.csv", stringsAsFactors = FALSE)
# users_urls_api <- users_urls_api %>% extract(col = url, into = "user_id",
#                                              regex = "users/(\\d+)\\?", remove = FALSE)
# 
# users_urls_api <- users_urls_api %>%
#   filter(!(user_id %in% unique(users_info$user_id)))
# 
# users_urls_api$user_id <- NULL
# 
# # # update file
# # setwd("C:/Users/au517585/Desktop/Projects/Stack_Exchange/motivation_feedback/data/Answers")
# # write.csv(users_urls_api, "./users_urls_api.csv", row.names = FALSE)


# ## Get Community info
# setwd("C:/Users/au517585/Desktop/Projects/Stack_Exchange/motivation_feedback/data/Answers")
# communities_info <- read.csv(file="./communities_info.csv", stringsAsFactors = FALSE)
# 
# communities_info$launch_date <- as.POSIXct(communities_info$launch_date,
#                             origin="1970-01-01",
#                             tz='UTC')
# 
# communities_info$open_beta_date <- as.POSIXct(communities_info$open_beta_date,
#                                            origin="1970-01-01",
#                                            tz='UTC')
# 
# communities_info$closed_beta_date <- as.POSIXct(communities_info$closed_beta_date,
#                                               origin="1970-01-01",
#                                               tz='UTC')

# Accepted Answerv
# Import file
setwd("C:/Users/au517585/Desktop/Projects/Stack_Exchange/motivation_feedback/data")
d.ux.q.00 <- read.csv(file="./d.ux.q.00.csv", stringsAsFactors=FALSE)

data_str_a <- merge(data_str_a, d.ux.q.00[, c("Id", "AcceptedAnswerId")], 
                    by.x = "ParentId", by.y = "Id", all.x = TRUE)

data_str_a$AcceptedAnswerDummy <- ifelse(data_str_a$Id == data_str_a$AcceptedAnswerId, 1, 0)
data_str_a$AcceptedAnswerDummy <- ifelse(is.na(data_str_a$AcceptedAnswerDummy), 0, data_str_a$AcceptedAnswerDummy)
data_str_a$AcceptedAnswerDummy <- factor(data_str_a$AcceptedAnswerDummy)

# For ALL 

data_str_all <- merge(data_str_all, d.ux.q.00[, c("Id", "AcceptedAnswerId")], 
                    by.x = "ParentId", by.y = "Id", all.x = TRUE)

data_str_all$AcceptedAnswerDummy <- ifelse(data_str_all$Id == data_str_all$AcceptedAnswerId, 1, 0)
data_str_all$AcceptedAnswerDummy <- ifelse(is.na(data_str_all$AcceptedAnswerDummy), 0, data_str_all$AcceptedAnswerDummy)
data_str_all$AcceptedAnswerDummy <- factor(data_str_all$AcceptedAnswerDummy)


model_pwp_gt_00 = coxph(Surv(tstop-tstart,status) ~
                                UpMod +
                                DownMod +
                                CommentCount +
                                AcceptedAnswerDummy + 
                                EditDummy + 
                                year + 
                                cluster(OwnerUserId) + strata(event), data=data_str_a, robust = TRUE)

summary(model_pwp_gt_00)

model_pwp_tt_00 = coxph(Surv(tstart, tstop ,status) ~
                          UpMod +
                          DownMod +
                          CommentCount +
                          AcceptedAnswerDummy + 
                          EditDummy + 
                          year + 
                          cluster(OwnerUserId) + strata(event), data=data_str_all, robust = TRUE)

summary(model_pwp_tt_00)


# p <- ggplot(data_str_a, aes(x = status, y = score, group = status, fill = status)) + # group = Close,
#   geom_violin(trim=FALSE)
# 
# p
# 
# data_str_a$status <- factor(data_str_a$status)
# data_str_a$score  <- as.numeric(levels(data_str_a$score))[data_str_a$score]
# # all
# p <- ggplot(data_str_a, aes(status, score))
# p + geom_boxplot() + ggtitle("ALL EVENTS")
# 
# # event by event. 54%
# data_str_a_event1 <- subset(data_str_a, event == 1)
# p <- ggplot(data_str_a_event1, aes(status, up_votes_ratio))
# p + geom_violin() + ggtitle("EVENT N. 1")
# 
# data_str_a_event2 <- subset(data_str_a, event == 2)
# p <- ggplot(data_str_a_event2, aes(status, score))
# p + geom_boxplot() + ggtitle("EVENT N. 2")


#https://rdrr.io/cran/ivtools/man/ivcoxph.html





