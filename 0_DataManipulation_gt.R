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

# Date Formatting 
tmp_history$CreationDate <- as.POSIXct(tmp_history$CreationDate, format = "%Y-%m-%d %H:%M:%S", tz = "UTC")
tmp_history$EditDate <- as.POSIXct(tmp_history$EditDate, format = "%Y-%m-%d %H:%M:%S", tz = "UTC")

# Assign fictitious UserId number for NA
tmp_history$UserId[is.na(tmp_history$UserId)] <- -99
# Add fictitious EditDate for NA
tmp_history$EditDate[is.na(tmp_history$EditDate)] <- as.POSIXct("2100-01-01")

# With GT: each answer has the count of all edits made 
# by others on the previous answer

# Initialize the dataframe and the row count
EditCount_df <- data.frame(Id = as.numeric(), 
                 EditCount = as.numeric(),
                 stringsAsFactors=FALSE) 
row = 1

# 1. For all the users
for (i in unique(tmp_history$OwnerUserId)) {
  tmp <- subset(tmp_history, OwnerUserId == i)
  tmp <- tmp %>% arrange(CreationDate)
  post_id <- unique(tmp$Id)
  # 2. For each answer check how many Edits are made on the 
  # previous answer before the current answer CreationDate
  for (n in seq_along(post_id)) {
    # if this is the first post no edits to count
    if (post_id[[n]] == head(post_id, n=1)) {
      EditCount_df[row, 1] <- post_id[[n]]
      EditCount_df[row, 2] <- 0
      row = row + 1
    } else {
      # Store the result in the dataframe
      EditCount_df[row, 1] <- post_id[[n]]
      EditCount_df[row, 2] <- sum(unique(tmp$CreationDate[tmp$Id == post_id[[n]]]) > 
                                    (tmp$EditDate[tmp$Id == post_id[[n-1]]]) & 
                                    (tmp$UserId[tmp$Id == post_id[[n-1]]] != i))
      row = row + 1
    }
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

# Assign fictitious VoteTypeId number for NA
tmp_Votes$VoteTypeId[is.na(tmp_Votes$VoteTypeId)] <- -99
# Add fictitious VoteCreationDate for NA
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
  post_id <- unique(tmp$Id)
  # 2. For each answer check how many AcceptedByOriginator, UpMod and DownMod
  # are made on the previous answer before the current answer CreationDate
  for (n in seq_along(post_id)) {
    # if this is the first post no votes to count
    if (post_id[[n]] == head(post_id, n=1)) {
      VoteCount_df[row, 1] <- post_id[[n]]
      VoteCount_df[row, 2] <- 0
      VoteCount_df[row, 3] <- 0
      VoteCount_df[row, 4] <- 0
      row = row + 1
    } else {
      # Store the result in the dataframe
      VoteCount_df[row, 1] <- post_id[[n]]
      VoteCount_df[row, 2] <- sum(unique(tmp$CreationDate[tmp$Id == post_id[[n]]]) > 
                                    (tmp$VoteCreationDate[tmp$Id == post_id[[n-1]]]) & 
                                    (tmp$VoteTypeId[tmp$Id == post_id[[n-1]]] == 1))
      VoteCount_df[row, 3] <- sum(unique(tmp$CreationDate[tmp$Id == post_id[[n]]]) > 
                                    (tmp$VoteCreationDate[tmp$Id == post_id[[n-1]]]) & 
                                    (tmp$VoteTypeId[tmp$Id == post_id[[n-1]]] == 2))
      VoteCount_df[row, 4] <- sum(unique(tmp$CreationDate[tmp$Id == post_id[[n]]]) > 
                                    (tmp$VoteCreationDate[tmp$Id == post_id[[n-1]]]) & 
                                    (tmp$VoteTypeId[tmp$Id == post_id[[n-1]]] == 3))
      row = row + 1
    }
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

# Add fictitious VoteCreationDate for NA
tmp_Comments$CommentCreationDate[is.na(tmp_Comments$CommentCreationDate)] <- as.POSIXct("2100-01-01")

# With GT: each answer has the count of all comments made 
# on the previous answer

# Initialize the dataframe and the row count
CommentCount_df <- data.frame(Id = as.numeric(), 
                           CommentCount = as.numeric(),
                           stringsAsFactors=FALSE) 
row = 1

# 1. For all the users
for (i in unique(tmp_Comments$OwnerUserId)) {
  tmp <- subset(tmp_Comments, OwnerUserId == i)
  tmp <- tmp %>% arrange(CreationDate)
  post_id <- unique(tmp$Id)
  # 2. For each answer check how many Comments are made on the 
  # previous answer before the current answer CreationDate
  for (n in seq_along(post_id)) {
    # if this is the first post no Comments to count
    if (post_id[[n]] == head(post_id, n=1)) {
      CommentCount_df[row, 1] <- post_id[[n]]
      CommentCount_df[row, 2] <- 0
      row = row + 1
    } else {
      # Store the result in the dataframe
      CommentCount_df[row, 1] <- post_id[[n]]
      CommentCount_df[row, 2] <- sum(unique(tmp$CreationDate[tmp$Id == post_id[[n]]]) > 
                                       tmp$CommentCreationDate[tmp$Id == post_id[[n-1]]])
      row = row + 1
    }
  }
}

rm(tmp, tmp_Comments, i, n, row)

data_str_all <- merge(data_str_all, CommentCount_df, 
                      by = "Id", all.x = TRUE)

# Save the file
setwd("C:/Projects/Stack_Exchange/motivation_feedback/Answers/data")
write.csv(data_str_all, "data_str_all_gt.csv", row.names = FALSE)




