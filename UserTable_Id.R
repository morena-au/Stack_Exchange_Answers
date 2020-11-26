## Collect USERS information
## Get OwnerUserId and query the User table

library(dplyr)

# Import file
setwd("C:/Projects/Stack_Exchange/motivation_feedback/data")
d.ux.a.00 <- read.csv(file="./d.ux.a.00.csv", stringsAsFactors=FALSE)
UserId <- unique(d.ux.a.00$OwnerUserId)

# Write down a text file with all the UserId separeted by a comma
# To make it easier to copy and paste the UserId in the WHERE statement

# SELECT *
#   FROM Users
# WHERE Id IN (6, 54, 2597);


# Data Explorer has a limit of 50000 rows that it can fetch in one call
# We split the UserId in order to obtain all the info
# # length(UserId)/4
# # 3022

write.table(paste(UserId[1:3022], collapse = ","),
            "C:/Projects/Stack_Exchange/motivation_feedback/Answers/data/old/Users/UserId.a.01.txt",
            sep = ",", row.names = FALSE, col.names = FALSE)

write.table(paste(UserId[3023:6044], collapse = ","),
            "C:/Projects/Stack_Exchange/motivation_feedback/Answers/data/old/Users/UserId.a.01.txt",
            sep = ",", row.names = FALSE, col.names = FALSE)

write.table(paste(UserId[6045:9066], collapse = ","),
            "C:/Projects/Stack_Exchange/motivation_feedback/Answers/data/old/Users/UserId.a.01.txt",
            sep = ",", row.names = FALSE, col.names = FALSE)

write.table(paste(UserId[9067:12088], collapse = ","),
            "C:/Projects/Stack_Exchange/motivation_feedback/Answers/data/old/Users/UserId.a.01.txt",
            sep = ",", row.names = FALSE, col.names = FALSE)

# Once downloaded merge them together and save in one file
# Get users information from Users Table in SE Data Explorer
QueryUsers01 <- read.csv(file="./old/Users/QueryUsers.a.01.csv",stringsAsFactors=FALSE)
QueryUsers02 <- read.csv(file="./old/Users/QueryUsers.a.02.csv",stringsAsFactors=FALSE)
QueryUsers03 <- read.csv(file="./old/Users/QueryUsers.a.03.csv",stringsAsFactors=FALSE)
QueryUsers04 <- read.csv(file="./old/Users/QueryUsers.a.04.csv",stringsAsFactors=FALSE)

# Append user query
Users <- rbind(QueryUsers01, QueryUsers02, QueryUsers03, QueryUsers04)
rm(QueryUsers01, QueryUsers02, QueryUsers03, QueryUsers04)

write.csv(Users, "Users.csv", row.names = FALSE)







