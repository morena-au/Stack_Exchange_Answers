## SURVIVAL ANALYSIS USEREXPERIENCE ANSWERS
library(plyr)
library(dplyr)
library(tidyr)


# Import file
setwd("C:/Projects/Stack_Exchange/motivation_feedback/Answers/data/raw")
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
d.ux.a.01$CreationDate <- as.POSIXct(d.ux.a.01$CreationDate, 
                                     format = "%Y-%m-%d %H:%M:%S", tz = "UTC")
d.ux.a.02 <- d.ux.a.01[d.ux.a.01$CreationDate < as.POSIXct("2019-01-01"), ]

#DataFormatting
d.ux.a.02$LastAccessDate <- as.POSIXct(d.ux.a.02$LastAccessDate, 
                                       format = "%Y-%m-%d %H:%M:%S", tz = "UTC")

# Database Errors some posts from the same user have different LastAccessDate
# Assign the oldest access date to all the posts by an user 
UniqueLastAccess <- d.ux.a.02 %>%
  group_by(OwnerUserId) %>%
  arrange(LastAccessDate) %>%
  filter(row_number()==n()) %>%
  select(Id, OwnerUserId, LastAccessDate)

d.ux.a.02$LastAccessDate <- NULL
d.ux.a.02 <- merge(d.ux.a.02, UniqueLastAccess[, c("OwnerUserId", "LastAccessDate")], 
                   by = "OwnerUserId", all.x = TRUE)

## DATA STRUCTURE FOR MODELLING RECURRENT EVENT DATA
tmp <- d.ux.a.02 %>%
  group_by(OwnerUserId) %>%
  arrange(CreationDate) %>%
  mutate(tstart = as.numeric(difftime(CreationDate, lag(CreationDate), tz = "UTC"), units = "secs")) %>%
  mutate(tstop = as.numeric(difftime(lead(CreationDate), CreationDate, tz = "UTC"), units = "secs")) %>%
  mutate(event = row_number())

tmp$status <- ifelse(is.na(tmp$tstop), 0, 1)


# End date for censured observations
tmp$test <- ifelse(tmp$LastAccessDate <= as.POSIXct("2019-01-01"), # contributor lost before the end of the study
                   as.numeric(difftime(tmp$LastAccessDate, tmp$CreationDate, tz = "UTC"), units = "secs"),
                   as.numeric(difftime(as.POSIXct("2019-01-01"), tmp$CreationDate, tz = "UTC"), units = "secs"))

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
  tmp <- tmp %>% arrange(CreationDate)
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

# Get the cumulative votes up, down and answers accepted by the originator 
# for all the answers by the author until previous to the current answer date

tmp_Votes <- data_str_all %>%
  select(c("OwnerUserId", "Id", "CreationDate"))


tmp_Votes <- merge(tmp_Votes, Votes[, c("PostId", "VoteTypeId", "VoteName",
                                        "VoteCreationDate")], by.x = "Id", by.y = "PostId")

# Keep only AcceptedByOriginator, UpMod and DownMod
tmp_Votes <- subset(tmp_Votes, VoteTypeId %in% c(1, 2, 3))


# Date Formatting 
tmp_Votes$CreationDate <- as.POSIXct(tmp_Votes$CreationDate, format = "%Y-%m-%d %H:%M:%S", tz = "UTC")
# Consider votes as they were given at the end of the day
# Approx: so no future votes are given to the current answers (avoid reverse causality)
tmp_Votes$VoteCreationDate <- as.POSIXct(tmp_Votes$VoteCreationDate, 
                                         format = "%Y-%m-%d %H:%M:%S", tz = "UTC") + 23*60*60 + 59*60 + 59

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
  tmp <- tmp %>% arrange(CreationDate)
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


# Get cumulative comments count before the current answer
tmp_Comments <- data_str_all %>%
  select(c("OwnerUserId", "Id", "CreationDate"))

tmp_Comments <- merge(tmp_Comments, Comments[, c("PostId", "CommentCreationDate")], 
                      by.x = "Id", by.y = "PostId")

# Date Formatting 
tmp_Comments$CreationDate <- as.POSIXct(tmp_Comments$CreationDate, 
                                        format = "%Y-%m-%d %H:%M:%S", tz = "UTC")
tmp_Comments$CommentCreationDate <- as.POSIXct(tmp_Comments$CommentCreationDate, 
                                               format = "%Y-%m-%d %H:%M:%S", tz = "UTC")

# With TT: each answer has the count of all comments made on all the posts
# wrote by the author previous to the current answer


# Initialize the dataframe and the row count
CommentCount_df <- data.frame(Id = as.numeric(), 
                           CommentCount = as.numeric(),
                           stringsAsFactors=FALSE) 
row = 1

# 1. For all the users
for (i in unique(tmp_Comments$OwnerUserId)) {
  tmp <- subset(tmp_Comments, OwnerUserId == i)
  tmp <- tmp %>% arrange(CreationDate)
  # 2. For each answer check how many Comments come before
  # the current answer CreationDate
  for (n in unique(tmp$Id)) {
    # Store the result in the dataframe
    CommentCount_df[row, 1] <- n
    CommentCount_df[row, 2] <- sum(unique(tmp$CreationDate[tmp$Id == n]) > tmp$CommentCreationDate)
    row = row + 1
  }
}

rm(tmp, tmp_Comments, i, n, row)

data_str_all <- merge(data_str_all, CommentCount_df, 
                      by = "Id", all.x = TRUE)

# Adjust for missing values
# if event = 1 and CommentsCount is missing >> 0 no comments on the first answer
data_str_all$CommentCount[data_str_all$event == 1 & is.na(data_str_all$CommentCount)] <- 0

data_str_all <- data_str_all %>%
  arrange(OwnerUserId, event)

# 1. For all the users
for (i in unique(data_str_all$OwnerUserId)) {
  # 2. For all the events
  for (n in data_str_all[(data_str_all$OwnerUserId == i), "event"]) {
    # if CommentCount is nan copy the value of CommentCount from previous event
    if (is.na(data_str_all[(data_str_all$event == n &
                            data_str_all$OwnerUserId == i), "CommentCount"])) {
      data_str_all[(data_str_all$event == n &
                      data_str_all$OwnerUserId == i),
                   "CommentCount"] <- data_str_all[(data_str_all$event == n-1 &
                                                   data_str_all$OwnerUserId == i), "CommentCount"]
    }
  }
}


# Save the file
setwd("C:/Projects/Stack_Exchange/motivation_feedback/Answers/data")
write.csv(data_str_all, "data_str_all.csv", row.names = FALSE)




