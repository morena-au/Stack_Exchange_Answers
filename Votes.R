## Collect VOTES information
## Get OwnerUserId and query the User table

library(dplyr)

# Import file
setwd("C:/Projects/Stack_Exchange/motivation_feedback/data")
d.ux.a.00 <- read.csv(file="./d.ux.a.00.csv", stringsAsFactors=FALSE)
UserId <- unique(d.ux.a.00$OwnerUserId)

# Write down a text file with all the UserId separeted by a comma
# To make it easier to copy and paste the UserId in the WHERE statement

# see SQL_query.txt 

# Collect VOTES information
# Split UsersId since query reach the 50000
write.table(paste(UserId[1:755], collapse = ","),
            "C:/Projects/Stack_Exchange/motivation_feedback/Answers/data/old/Users/UserId.a.01A.txt",
            sep = ",", row.names = FALSE, col.names = FALSE)

write.table(paste(UserId[756:1132], collapse = ","),
            "C:/Projects/Stack_Exchange/motivation_feedback/Answers/data/old/Users/UserId.a.01Ba.txt",
            sep = ",", row.names = FALSE, col.names = FALSE)

write.table(paste(UserId[1133:1510], collapse = ","),
            "C:/Projects/Stack_Exchange/motivation_feedback/Answers/data/old/Users/UserId.a.01Bb.txt",
            sep = ",", row.names = FALSE, col.names = FALSE)

write.table(paste(UserId[1511:2265], collapse = ","),
            "C:/Projects/Stack_Exchange/motivation_feedback/Answers/data/old/Users/UserId.a.01C.txt",
            sep = ",", row.names = FALSE, col.names = FALSE)

write.table(paste(UserId[2266:3022], collapse = ","),
            "C:/Projects/Stack_Exchange/motivation_feedback/Answers/data/old/Users/UserId.a.01D.txt",
            sep = ",", row.names = FALSE, col.names = FALSE)
write.table(paste(UserId[3023:3778], collapse = ","),
            "C:/Projects/Stack_Exchange/motivation_feedback/Answers/data/old/Users/UserId.a.02A.txt",
            sep = ",", row.names = FALSE, col.names = FALSE)

write.table(paste(UserId[3779:4534], collapse = ","),
            "C:/Projects/Stack_Exchange/motivation_feedback/Answers/data/old/Users/UserId.a.02B.txt",
            sep = ",", row.names = FALSE, col.names = FALSE)

write.table(paste(UserId[4535:5290], collapse = ","),
            "C:/Projects/Stack_Exchange/motivation_feedback/Answers/data/old/Users/UserId.a.02C.txt",
            sep = ",", row.names = FALSE, col.names = FALSE)

write.table(paste(UserId[5291:6044], collapse = ","),
            "C:/Projects/Stack_Exchange/motivation_feedback/Answers/data/old/Users/UserId.a.02D.txt",
            sep = ",", row.names = FALSE, col.names = FALSE)
write.table(paste(UserId[6045:7555], collapse = ","),
            "C:/Projects/Stack_Exchange/motivation_feedback/Answers/data/old/Users/UserId.a.03A.txt",
            sep = ",", row.names = FALSE, col.names = FALSE)

write.table(paste(UserId[7556:9066], collapse = ","),
            "C:/Projects/Stack_Exchange/motivation_feedback/Answers/data/old/Users/UserId.a.03B.txt",
            sep = ",", row.names = FALSE, col.names = FALSE)

# Once downloaded merge them together and save in one file
# Import Votes by Post 
VotesPostDate_01_a <- read.csv(file="./old/Votes/QueryVotes_01A.csv",stringsAsFactors=FALSE)
VotesPostDate_01_ba <- read.csv(file="./old/Votes/QueryVotes_01Ba.csv",stringsAsFactors=FALSE)
VotesPostDate_01_bb <- read.csv(file="./old/Votes/QueryVotes_01Bb.csv",stringsAsFactors=FALSE)
VotesPostDate_01_c <- read.csv(file="./old/Votes/QueryVotes_01C.csv",stringsAsFactors=FALSE)
VotesPostDate_01_d <- read.csv(file="./old/Votes/QueryVotes_01D.csv",stringsAsFactors=FALSE)

VotesPostDate_02_a <- read.csv(file="./old/Votes/QueryVotes_02A.csv",stringsAsFactors=FALSE)
VotesPostDate_02_b <- read.csv(file="./old/Votes/QueryVotes_02B.csv",stringsAsFactors=FALSE)
VotesPostDate_02_c <- read.csv(file="./old/Votes/QueryVotes_02C.csv",stringsAsFactors=FALSE)
VotesPostDate_02_d <- read.csv(file="./old/Votes/QueryVotes_02D.csv",stringsAsFactors=FALSE)

VotesPostDate_03_a <- read.csv(file="./old/Votes/QueryVotes_03A.csv",stringsAsFactors=FALSE)
VotesPostDate_03_b <- read.csv(file="./old/Votes/QueryVotes_03B.csv",stringsAsFactors=FALSE)

VotesPostDate_04 <- read.csv(file="./old/Votes/QueryVotes_04.csv",stringsAsFactors=FALSE)

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

write.csv(Votes, "Votes.csv", row.names = FALSE)
