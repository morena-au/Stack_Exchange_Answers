## SURVIVAL ANALYSIS USEREXPERIENCE ANSWERS
library(plyr)
library(dplyr)
library(tidyr)


# Import file
setwd("C:/Projects/Stack_Exchange/motivation_feedback/Answers/data")
d.ux.a.00 <- read.csv(file="./d.ux.a.00.csv", stringsAsFactors=FALSE)

keep <- c("ParentId", "Id", "CreationDate", "OwnerUserId",
          "LastActivityDate")

d.ux.a.01 <- d.ux.a.00[keep]

# Get User Info
Users <- read.csv(file="./Users.csv",stringsAsFactors=FALSE)

# Merge them together
d.ux.a.01 <- merge(d.ux.a.01, Users[, c("Id", "LastAccessDate", "AccountId")], 
                   by.x = "OwnerUserId", by.y = "Id", all.x = TRUE)
# 73 users have been deleted from SE

d.ux.a.01$LastAccessDate <- ifelse(is.na(d.ux.a.01$LastAccessDate), d.ux.a.01$LastActivityDate, 
                                   d.ux.a.01$LastAccessDate)

# Get VOTES information
Votes <- read.csv(file="./Votes.csv",stringsAsFactors=FALSE)

# Get Comments by Post and Date
Comments <- read.csv(file="./Comments.csv",stringsAsFactors=FALSE)

# Extract Answers before 01/01/2019
d.ux.a.02 <- d.ux.a.01[d.ux.a.01$CreationDate < "2019-01-01 00:00:00", ]


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

# consider the full history 
data_str_all <- data.frame(tmp)
rm(tmp)

## ADD FURTHER VARIABLES
# Add one secs to tstop where tstart and tstop are equal (TT)
data_str_all$tstop <- ifelse(data_str_all$tstart == data_str_all$tstop, 
                             data_str_all$tstop + 1, data_str_all$tstop)

# Consider the time between answers as output
data_str_all$TimeBetweenAnswers <- data_str_all$tstop - data_str_all$tstart

# Consider all the changes the answer went through before the next post
PostHistory <- read.csv(file="./PostHistory.csv",stringsAsFactors=FALSE)
PostHistoryTypes <- read.csv("./PostHistoryTypes.csv", stringsAsFactors = FALSE)

keep <- c("PostHistoryTypeId", "PostId", "CreationDate", "UserId")
PostHistory <- PostHistory[keep]
PostHistory <- merge(PostHistory, PostHistoryTypes, 
                     by.x = "PostHistoryTypeId", by.y = "Id", all.x = TRUE)
rm(PostHistoryTypes)

# Keep only Edit Body
PostHistory <- subset(PostHistory, PostHistoryTypeId == 5)
colnames(PostHistory)[3] <- "EditDate"

tmp_history <- data_str_all %>%
  select(c("OwnerUserId", "Id", "CreationDate"))

tmp_history <- merge(tmp_history, PostHistory, by.x = "Id", by.y = "PostId", all.x = TRUE)

# Remove edits made by the same author
tmp_history <- subset(tmp_history, OwnerUserId != UserId)

# Date Formatting 
tmp_history$CreationDate <- as.POSIXct(tmp_history$CreationDate, format = "%Y-%m-%d %H:%M:%S", tz = "UTC")
tmp_history$EditDate <- as.POSIXct(tmp_history$EditDate, format = "%Y-%m-%d %H:%M:%S", tz = "UTC")

# With TT: each answer has the count of all edits made on all the posts
# wrote by the author by others previous to the current answer


# Initialize the dataframe and the row count
EditCount_df <- data.frame(Id = as.numeric(), 
                 EditCount = as.numeric(),
                 stringsAsFactors=FALSE) 
row = 1

# 1. For all the users
for (i in unique(tmp_history$OwnerUserId)) {
  tmp <- subset(tmp_history, OwnerUserId == i)
  # 2. For each answer check how many Edits come before
  # than the current answer CreationDate
  for (n in unique(tmp$Id)) {
    # Store the result in the dataframe
    EditCount_df[row, 1] <- n
    EditCount_df[row, 2] <- sum(unique(tmp$CreationDate[tmp$Id == n]) > tmp$EditDate)
    row = row + 1
  }
}

rm(tmp, tmp_history, i, keep, n, row)

data_str_all <- merge(data_str_all, EditCount_df, 
                      by = "Id", all.x = TRUE)

# Adjust for missing values
# if event = 1, EditCount is missing >> 0 no edits on the first answer
data_str_all$EditCount[data_str_all$event == 1 & is.na(data_str_all$EditCount)] <- 0

data_str_all <- data_str_all %>%
  arrange(OwnerUserId, event)

# 1. For all the users
for (i in unique(data_str_all$OwnerUserId)) {
  # 2. For all the events
  for (n in data_str_all[(data_str_all$OwnerUserId == i), "event"]) {
    # if EditCount is nan copy the value of EditCount from previous event
    if (is.na(data_str_all[(data_str_all$event == n &
                            data_str_all$OwnerUserId == i), "EditCount"])) {
      data_str_all[(data_str_all$event == n &
                      data_str_all$OwnerUserId == i),
                   "EditCount"] <- data_str_all[(data_str_all$event == n-1 &
                                                   data_str_all$OwnerUserId == i), "EditCount"]
    }
  }
}

# Get the accumulative votes up, down until for all the answers by the author
# until previously the current answer date

tmp_Votes <- data_str_all %>%
  select(c("OwnerUserId", "Id", "CreationDate"))


tmp_Votes <- merge(tmp_Votes, Votes[, c("PostId", "VoteTypeId", "VoteName",
                                        "VoteCreationDate")], by.x = "Id", by.y = "PostId")

# Keep only AcceptedByOriginator, UpMod and DownMod
tmp_Votes <- subset(tmp_Votes, VoteTypeId %in% c(1, 2, 3))


# Date Formatting 
tmp_Votes$CreationDate <- as.POSIXct(tmp_Votes$CreationDate, format = "%Y-%m-%d %H:%M:%S", tz = "UTC")
# Consider votes as they were given at the end of the day
# Approx: so no future votes are given to answers (avoid reverse causality)
tmp_Votes$VoteCreationDate <- as.POSIXct(tmp_Votes$VoteCreationDate, format = "%Y-%m-%d %H:%M:%S", tz = "UTC") + 23*60*60 + 59*60 + 59

# Initialize the dataframe and the row count
VoteCount_df <- data.frame(Id = as.numeric(), 
                           AcceptedByOriginator = as.numeric(),
                           UpMod = as.numeric(), 
                           DownMod = as.numeric(),
                           stringsAsFactors=FALSE) 
row = 1

# 1. For all the users
for (i in unique(tmp_Votes$OwnerUserId)) {
  tmp <- subset(tmp_Votes, OwnerUserId == i)
  # 2. For each answer check how many AcceptedByOriginator, UpMod and DownMod that
  # come before than the current answer CreationDate
  for (n in unique(tmp$Id)) {
    # Store the result in the dataframe
    VoteCount_df[row, 1] <- n
    VoteCount_df[row, 2] <- sum(unique(tmp$CreationDate[tmp$Id == n]) > tmp$VoteCreationDate & 
                                  tmp$VoteTypeId == 1)
    VoteCount_df[row, 3] <- sum(unique(tmp$CreationDate[tmp$Id == n]) > tmp$VoteCreationDate & 
                                  tmp$VoteTypeId == 2)
    VoteCount_df[row, 4] <- sum(unique(tmp$CreationDate[tmp$Id == n]) > tmp$VoteCreationDate &
                                  tmp$VoteTypeId == 3)
    row = row + 1
  }
}

rm(i, n, row, tmp, tmp_Votes)


data_str_all <- merge(data_str_all, VoteCount_df, 
                      by = "Id", all.x = TRUE)

# Adjust for missing values
data_str_all <- data_str_all %>%
  arrange(OwnerUserId, event)

# if event = 1, AcceptedByOriginator is missing >> no AcceptedByOriginator on the first answer
data_str_all$AcceptedByOriginator[data_str_all$event == 1 & is.na(data_str_all$AcceptedByOriginator)] <- 0
# 1. For all the users
for (i in unique(data_str_all$OwnerUserId)) {
  # 2. For all the events
  for (n in data_str_all[(data_str_all$OwnerUserId == i), "event"]) {
    # if AcceptedByOriginator is nan copy the value of AcceptedByOriginator from previous event
    if (is.na(data_str_all[(data_str_all$event == n &
                            data_str_all$OwnerUserId == i), "AcceptedByOriginator"])) {
      data_str_all[(data_str_all$event == n &
                      data_str_all$OwnerUserId == i),
                   "AcceptedByOriginator"] <- data_str_all[(data_str_all$event == n-1 &
                                                   data_str_all$OwnerUserId == i), "AcceptedByOriginator"]
    }
  }
}

# if event = 1, UpMod is missing>> no UpMod on the first answer
data_str_all$UpMod[data_str_all$event == 1 & is.na(data_str_all$UpMod)] <- 0
# 1. For all the users
for (i in unique(data_str_all$OwnerUserId)) {
  # 2. For all the events
  for (n in data_str_all[(data_str_all$OwnerUserId == i), "event"]) {
    # if UpMod is nan copy the value of UpMod from previous event
    if (is.na(data_str_all[(data_str_all$event == n &
                            data_str_all$OwnerUserId == i), "UpMod"])) {
      data_str_all[(data_str_all$event == n &
                      data_str_all$OwnerUserId == i),
                   "UpMod"] <- data_str_all[(data_str_all$event == n-1 &
                                                              data_str_all$OwnerUserId == i), "UpMod"]
    }
  }
}

# if event = 1, DownMod is missing >> no DownMod on the first answer
data_str_all$DownMod[data_str_all$event == 1 & is.na(data_str_all$DownMod)] <- 0
# 1. For all the users
for (i in unique(data_str_all$OwnerUserId)) {
  # 2. For all the events
  for (n in data_str_all[(data_str_all$OwnerUserId == i), "event"]) {
    # if DownMod is nan copy the value of DownMod from previous event
    if (is.na(data_str_all[(data_str_all$event == n &
                            data_str_all$OwnerUserId == i), "DownMod"])) {
      data_str_all[(data_str_all$event == n &
                      data_str_all$OwnerUserId == i),
                   "DownMod"] <- data_str_all[(data_str_all$event == n-1 &
                                               data_str_all$OwnerUserId == i), "DownMod"]
    }
  }
}


#TODO: truncation is necessary also for PWP-TT (braga2018recurrent) 

# Save the file
write.csv(data_str_all, "data_str_all.csv", row.names = FALSE)

'We consider data from a study with 11778 contributors in User Experience
Stack Exchange. The study is designed to evaluate the effect of covariates
on answering a question again.
All contributors entered the study once they answered the first question.
The event of interest is answer recurrence.
Contributors were censored at the time last visit to the website is
older than 6 months (https://stackoverflow.blog/2009/02/16/when-is-an-account-abandoned/).

Contributors were followed until 2018-12-31. 
40% of the contributors had at least one recurrence (Answered at least 2 answers)
Median number of recurrences is 4, varying from 2 answers given to 919.
Among those with at least one recurrence, 79% had at most 10 recurrences.
We truncate the dataset after the tenth event due to the small number of events
in the later strata.'

length(unique(data_str_all$OwnerUserId))
user_freq <- count(data_str_all, OwnerUserId)
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

rm(rec3, rec4, rec5, rec6, rec7, rec8, rec9)


# Truncate observations after the tenth event
data_str_tr <- subset(data_str_all, event <= 10)

# Save the file
write.csv(data_str_tr, "data_str_tr.csv", row.names = FALSE)






# # Change them into factors
# data_str_a$EditDummy <- factor(data_str_a$EditDummy)
# data_str_all$EditDummy <- factor(data_str_all$EditDummy)

# TODO: should not be relevant since the minority of the answers are edited
# # Include in the test whether last edit was made by the asking user (LastEditorUser = 1)
# data_str_q$LastEditorUser <- ifelse(data_str_q$OwnerUserId == data_str_q$LastEditorUserId, 1, 0)
# # input 0 to all the missing > the focus is given to the user that made the edit
# data_str_q$LastEditorUser[is.na(data_str_q$LastEditorUser)] <- 0
# data_str_q$LastEditorUser <- factor(data_str_q$LastEditorUser)



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





