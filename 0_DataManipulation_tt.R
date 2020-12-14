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
UsersAccount <- read.csv(file="./users_info.csv",stringsAsFactors=FALSE)

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
data_str_all <- merge(data_str_all, UsersAccount[, c("is_employee",
                                                     "creation_date", "user_type", "user_id")], 
                      by.x = "OwnerUserId", by.y = "user_id", all.x = TRUE)

# # Amount of answers given by each user type
# data_str_all %>%
#   group_by(user_type) %>%
#   tally()

# Remove unregistered participants
data_str_all <- subset(data_str_all, user_type == "registered")

data_str_all$creation_date <- as.POSIXct(data_str_all$creation_date,
                                         origin="1970-01-01",
                                         tz='UTC')
# delete column
data_str_all$user_type <- NULL

# Remove for employees
data_str_all <- subset(data_str_all, is_employee != "True")
data_str_all$is_employee <- NULL

# Add one secs to tstop where tstart and tstop are equal (TT)
data_str_all$tstop <- ifelse(data_str_all$tstart == data_str_all$tstop, 
                             data_str_all$tstop + 1, data_str_all$tstop)

# Consider all the changes the answer went through before the next post
PostHistory <- read.csv(file="./PostHistory.csv",stringsAsFactors=FALSE)
PostHistoryTypes <- read.csv("./PostHistoryTypes.csv", stringsAsFactors = FALSE)

keep <- c("PostHistoryTypeId", "PostId", "CreationDate", "UserId")
PostHistory <- PostHistory[keep]
PostHistory <- merge(PostHistory, PostHistoryTypes, 
                     by.x = "PostHistoryTypeId", by.y = "Id", all.x = TRUE)
colnames(PostHistory)[3] <- "EditDate"
rm(PostHistoryTypes)

tmp_history <- data_str_all %>%
  select(c("OwnerUserId", "Id", "CreationDate"))

tmp_history <- merge(tmp_history, PostHistory, by.x = "Id", by.y = "PostId", all.x = TRUE)

# Date Formatting 
tmp_history$CreationDate <- as.POSIXct(tmp_history$CreationDate, format = "%Y-%m-%d %H:%M:%S", tz = "UTC")
tmp_history$EditDate <- as.POSIXct(tmp_history$EditDate, format = "%Y-%m-%d %H:%M:%S", tz = "UTC")

# Assign fictitious data to NA
tmp_history$UserId[is.na(tmp_history$UserId)] <- -99
tmp_history$EditDate[is.na(tmp_history$EditDate)] <- as.POSIXct("2100-01-01")


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
    EditCount_df[row, 2] <- sum(unique(tmp$CreationDate[tmp$Id == n]) > tmp$EditDate &
                                  tmp$PostHistoryTypeId == 5 & tmp$UserId != tmp$OwnerUserId)
    row = row + 1
  }
}

rm(tmp, tmp_history, i, keep, n, row)

data_str_all <- merge(data_str_all, EditCount_df, 
                      by = "Id", all.x = TRUE)

# Get the cumulative votes up, down and answers accepted by the originator 
# for all the answers by the author until previous to the current answer date

tmp_Votes <- data_str_all %>%
  select(c("OwnerUserId", "Id", "CreationDate"))


tmp_Votes <- merge(tmp_Votes, Votes[, c("PostId", "VoteTypeId", "VoteName",
                                        "VoteCreationDate")], by.x = "Id", 
                                        by.y = "PostId", all.x = TRUE)

# Date Formatting 
tmp_Votes$CreationDate <- as.POSIXct(tmp_Votes$CreationDate, format = "%Y-%m-%d %H:%M:%S", tz = "UTC")
# Consider votes as they were given at the end of the day
# Approx: so no future votes are given to the current answers (avoid reverse causality)
tmp_Votes$VoteCreationDate <- as.POSIXct(tmp_Votes$VoteCreationDate, 
                                         format = "%Y-%m-%d %H:%M:%S", tz = "UTC") + 23*60*60 + 59*60 + 59

# Assign fictitious data to NA
tmp_Votes$VoteTypeId[is.na(tmp_Votes$VoteTypeId)] <- -99
tmp_Votes$VoteCreationDate[is.na(tmp_Votes$VoteCreationDate)] <- as.POSIXct("2100-01-01")


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


# Get cumulative comments count before the current answer
tmp_Comments <- data_str_all %>%
  select(c("OwnerUserId", "Id", "CreationDate"))

tmp_Comments <- merge(tmp_Comments, Comments[, c("PostId", "CommentCreationDate")], 
                      by.x = "Id", by.y = "PostId", all.x = TRUE)

# Date Formatting 
tmp_Comments$CreationDate <- as.POSIXct(tmp_Comments$CreationDate, 
                                        format = "%Y-%m-%d %H:%M:%S", tz = "UTC")
tmp_Comments$CommentCreationDate <- as.POSIXct(tmp_Comments$CommentCreationDate, 
                                               format = "%Y-%m-%d %H:%M:%S", tz = "UTC")


# Assign fictitious data to NA
tmp_Comments$CommentCreationDate[is.na(tmp_Comments$CommentCreationDate)] <- as.POSIXct("2100-01-01")

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
# Save the file
setwd("C:/Projects/Stack_Exchange/motivation_feedback/Answers/data")
write.csv(data_str_all, "data_str_all_tt.csv", row.names = FALSE)




