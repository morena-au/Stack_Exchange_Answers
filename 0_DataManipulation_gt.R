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

UsersAccount$creation_date <- as.POSIXct(UsersAccount$creation_date, 
                                         origin="1970-01-01",
                                         tz='UTC')

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
  mutate(tstart = as.numeric(difftime(CreationDate, lag(CreationDate), tz = "UTC"), units = "secs"),
         tstop = as.numeric(difftime(lead(CreationDate), CreationDate, tz = "UTC"), units = "secs"), 
         event = row_number())

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

data_str_all <- merge(data_str_all, UsersAccount[, c("user_id", "is_employee","user_type", "creation_date")], 
                     by.x = "OwnerUserId", by.y = "user_id", all.x = TRUE)

# # Amount of answers given by each user type
# data_str_all %>%
#   group_by(user_type) %>%
#   tally()

colnames(data_str_all)[which(names(data_str_all) == "creation_date")] <- "UX_registration"

# Remove unregistered participants
data_str_all <- subset(data_str_all, user_type == "registered")

# delete column
data_str_all$user_type <- NULL

# Remove for employees
data_str_all <- subset(data_str_all, is_employee != "True")
data_str_all$is_employee <- NULL


# Consider all the changes the answer went through before the next post
PostHistory <- read.csv(file="./PostHistory.csv",stringsAsFactors=FALSE)
PostHistoryTypes <- read.csv("./PostHistoryTypes.csv", stringsAsFactors = FALSE)

keep <- c("PostHistoryTypeId", "PostId", "CreationDate", "UserId")
PostHistory <- PostHistory[keep]
PostHistory <- merge(PostHistory, PostHistoryTypes, 
                     by.x = "PostHistoryTypeId", by.y = "Id", all.x = TRUE)
rm(PostHistoryTypes)

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
for (i in  seq(nrow(data_str_tr))) {
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
                                    (tmp$UserId[tmp$Id == post_id[[n-1]]] != i) &
                                    # Keep only Edit Body
                                    (tmp$PostHistoryTypeId[tmp$Id == post_id[[n-1]]] == 5))
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

tmp_Comments <- merge(tmp_Comments, Comments[, c("PostId", "CommentCreationDate", "CommentUserId")], 
                      by.x = "Id", by.y = "PostId", all.x = TRUE)

# Date Formatting 
tmp_Comments$CreationDate <- as.POSIXct(tmp_Comments$CreationDate, 
                                        format = "%Y-%m-%d %H:%M:%S", tz = "UTC")
tmp_Comments$CommentCreationDate <- as.POSIXct(tmp_Comments$CommentCreationDate, 
                                               format = "%Y-%m-%d %H:%M:%S", tz = "UTC")

# Add fictitious VoteCreationDate for NA
tmp_Comments$CommentCreationDate[is.na(tmp_Comments$CommentCreationDate)] <- as.POSIXct("2100-01-01")
tmp_Comments$CommentUserId[is.na(tmp_Comments$CommentUserId)] <- -99

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
  #print(paste0("OwnerUserId: ", i))
  # 2. For each answer check how many Comments are made on the 
  # previous answer before the current answer CreationDate
  for (n in seq_along(post_id)) {
    # if this is the first post no Comments to count
    if (post_id[[n]] == head(post_id, n=1)) {
      #print(paste0("First post: ", post_id[[n]]))
      CommentCount_df[row, 1] <- post_id[[n]]
      CommentCount_df[row, 2] <- 0
      row = row + 1
    } else {
      #print(paste0("Post: ", post_id[[n]]))
      # Store the result in the dataframe
      CommentCount_df[row, 1] <- post_id[[n]]
      CommentCount_df[row, 2] <- sum(unique(tmp$CreationDate[tmp$Id == post_id[[n]]]) > 
                                       (tmp$CommentCreationDate[tmp$Id == post_id[[n-1]]]) &
                                       # filter for comments only for the other users
                                       (tmp$CommentUserId[tmp$Id == post_id[[n-1]]] != i))
      row = row + 1
    }
  }
  #print("=======================================")
}

rm(tmp, tmp_Comments, i, n, row)

data_str_all <- merge(data_str_all, CommentCount_df, 
                      by = "Id", all.x = TRUE)

# Get community info
setwd("C:/Projects/Stack_Exchange/motivation_feedback/Answers/data/raw")
communities_info <- read.csv(file="./communities_info.csv", stringsAsFactors = FALSE)
communities_info$launch_date <- as.POSIXct(communities_info$launch_date,
                                           origin="1970-01-01",
                                           tz='UTC')
community_info <- subset(communities_info, site_url == "https://ux.stackexchange.com")

# Get participants who answer for the first time after the official launch
before_launch_tmp <- data_str_all[data_str_all$CreationDate < community_info$launch_date, ]
data_str_all <- subset(data_str_all, !(OwnerUserId %in% before_launch_tmp$OwnerUserId))

# Control for time fix effect
data_str_all$year <- substring(data_str_all$CreationDate, 1, 4)
data_str_all$day <- weekdays(as.Date(data_str_all$CreationDate))


# Control for attitude and intentions > which community joined first
setwd("C:/Projects/Stack_Exchange/motivation_feedback/Answers/data/raw")
AssociatedInfo <- read.csv("users_associated_info.csv", stringsAsFactors = FALSE)

# Remove duplicate
AssociatedInfo <- data.frame(AssociatedInfo[!duplicated(AssociatedInfo), ])

AssociatedInfo$creation_date <- as.POSIXct(AssociatedInfo$creation_date,
                                           origin="1970-01-01",
                                           tz='UTC')
# check for missing values
tmp <- subset(data_str_all, !(data_str_all$AccountId %in% AssociatedInfo$account_id))
unique(tmp$OwnerUserId)

# remove users 52638 53220 > do not participate in any communities
data_str_all <- subset(data_str_all, !(OwnerUserId %in% c(52638, 53220)))

data_str_all$AccountId <- ifelse(is.na(data_str_all$AccountId), 3941049, 
                                    data_str_all$AccountId)

Associated_tmp <- AssociatedInfo %>%
  group_by(account_id) %>%
  arrange(creation_date) %>%
  filter(row_number() == 1)

# Merge them together
data_str_all <- merge(data_str_all, Associated_tmp[, c("account_id", "site_name", "creation_date")],
                   by.x = "AccountId", by.y = "account_id", all.x = TRUE)

colnames(data_str_all)[which(names(data_str_all) == "creation_date")] <- "SE_registration"

unique(data_str_all$site_name)

# Possible database errors:
# if the registration in UX in Users was done before the oldest registration in AssociatedInfo
# keep UX registration as starting point. 

data_str_all$start_UX <- ifelse(data_str_all$UX_registration <= data_str_all$SE_registration, 1, 0)
data_str_all$site_name <- NULL
data_str_all$start_tenure <- ifelse(data_str_all$UX_registration <= data_str_all$SE_registration, 
                                 data_str_all$UX_registration,
                                 data_str_all$SE_registration)

data_str_all$start_tenure <- as.POSIXct(data_str_all$start_tenure,
                                     origin="1970-01-01",
                                     tz='UTC')

data_str_all$tenure<- substring(data_str_all$start_tenure, 1, 4)
data_str_all$tenure<- as.numeric(data_str_all$tenure)

# Import question
setwd("C:/Projects/Stack_Exchange/motivation_feedback/Answers/data/raw")
d.ux.q.00 <- read.csv(file="./d.ux.q.00.csv", stringsAsFactors=FALSE)

keep <- c("Id", "CreationDate", "OwnerUserId", "Tags")
d.ux.q.00 <- d.ux.q.00[keep]
colnames(d.ux.q.00)[2] <- "QuestionCreationDate"
colnames(d.ux.q.00)[3] <- "QuestionUserId"

tags_tmp <- data.frame(Id = d.ux.q.00$Id, str_split(d.ux.q.00$Tags, "><", simplify = TRUE))
tags_tmp <- data.frame(tags_tmp[1], apply(tags_tmp[2:6], 2, function(x) gsub("[<>]",'',x)))

# question based on the first tag (same domain, exposure, most important)
tags_tmp <- tags_tmp[,1:2]
colnames(tags_tmp)[2] <- "QuestionTag"

# Merge them together
data_str_all <- merge(data_str_all, tags_tmp, 
                   by.x = "ParentId", by.y = "Id", all.x = TRUE)

# # keep all and order the tab alphabetically
# # Initialize the dataframe and the row count
# sorted_tags  <- data.frame(Id = as.numeric(), 
#                                 Tags = as.character(),
#                                 stringsAsFactors=FALSE) 
# row = 1
# 
# require(stringr)
# # 1. For all the row
# for (i in c(1:dim(tags_tmp)[1])) {
#   sorted_tags[row, 1] <- tags_tmp$Id[i]
#   sorted_tags[row, 2] <- str_trim(paste(sort(t(tags_tmp[i,2:6])), collapse = " "))
#   row = row + 1
# }
# 
# rm(keep, i, row)

# TODO randomly match question based on the same tag

# Save the file
setwd("C:/Projects/Stack_Exchange/motivation_feedback/Answers/data")
write.csv(data_str_all, "data_str_all_gt.csv", row.names = FALSE)




