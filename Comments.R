# COMMENTS BY ANSWERS AND DATE

library(dplyr)

# Import file
setwd("C:/Projects/Stack_Exchange/motivation_feedback/Answers/data/old/Comments")

# Use the UserId separeted by a comma in data/Users/
# To make it easier to copy and paste the UserId in the WHERE statement

# see SQL_query.txt 

# Get Comments by Post and Date
CommentDate_01 <- read.csv(file="./QueryComments_01.csv",stringsAsFactors=FALSE)
CommentDate_02 <- read.csv(file="./QueryComments_02.csv",stringsAsFactors=FALSE)
CommentDate_03 <- read.csv(file="./QueryComments_03.csv",stringsAsFactors=FALSE)
CommentDate_04 <- read.csv(file="./QueryComments_04.csv",stringsAsFactors=FALSE)

# Append user query
Comments <- rbind(CommentDate_01, CommentDate_02, CommentDate_03, CommentDate_04)
rm(CommentDate_01, CommentDate_02, CommentDate_03, CommentDate_04)

setwd("C:/Projects/Stack_Exchange/motivation_feedback/Answers/data")
write.csv(Comments, "./Comments.csv", row.names = FALSE)
