## Collect POST HISTORY information

library(dplyr)

# Adjust for reverse causality
# if edited before the next answer

# Import file
setwd("C:/Projects/Stack_Exchange/motivation_feedback/Answers/data")
data_str_all <- read.csv(file="./data_str_all.csv", stringsAsFactors=FALSE)
PostId <- unique(data_str_all$Id)

# Write down a text file with all the PostId separeted by a comma
# To make it easier to copy and paste the PostId in the WHERE statement
#Split PostId since query reach the 50000
# > length(PostId)/3
# [1] 23525

write.table(paste(PostId[1:23525], collapse = ","),
            "C:/Projects/Stack_Exchange/motivation_feedback/data/Answers/PostId_a1.txt",
            sep = ",", row.names = FALSE, col.names = FALSE)

write.table(paste(PostId[23526:47051], collapse = ","),
            "C:/Projects/Stack_Exchange/motivation_feedback/data/Answers/PostId_a2.txt",
            sep = ",", row.names = FALSE, col.names = FALSE)

write.table(paste(PostId[47052:70575], collapse = ","),
            "C:/Projects/Stack_Exchange/motivation_feedback/data/Answers/PostId_a3.txt",
            sep = ",", row.names = FALSE, col.names = FALSE)

# Once downloaded merge them together and save in one file
PostHistory_a1 <- read.csv(file="./PostHistory_a1.csv",stringsAsFactors=FALSE)
PostHistory_a2 <- read.csv(file="./PostHistory_a2.csv",stringsAsFactors=FALSE)
PostHistory_a3 <- read.csv(file="./PostHistory_a3.csv",stringsAsFactors=FALSE)

# Append user query
PostHistory <- rbind(PostHistory_a1, PostHistory_a2, PostHistory_a3)

rm(PostHistory_a1, PostHistory_a2, PostHistory_a3)

write.csv(PostHistory, "PostHistory.csv", row.names = FALSE)
