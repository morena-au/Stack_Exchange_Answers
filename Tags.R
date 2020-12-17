library(plyr)
library(dplyr)
library(tidyr)

# Import file
setwd("C:/Projects/Stack_Exchange/motivation_feedback/Answers/data")
data_str_all_tt <- read.csv(file="./data_str_all_tt.csv", stringsAsFactors=FALSE)

# Write down a table with the unique ParentId in order to query the PostTags
ParentId <- unique(data_str_all_tt$ParentId)

write.table(paste(ParentId[1:11075], collapse = ","),
            "./old/Tags/ParentId_01.txt",
            sep = ",", row.names = FALSE, col.names = FALSE)

write.table(paste(ParentId[11076:22150], collapse = ","),
            "./old/Tags/ParentId_02.txt",
            sep = ",", row.names = FALSE, col.names = FALSE)

# Once downloaded merge them together and save in one file
# Import Tags by question id (ParentId)
ParentId_01 <- read.csv(file="./old/Tags/ParentId_01.csv",stringsAsFactors=FALSE)
ParentId_02 <- read.csv(file="./old/Tags/ParentId_02.csv",stringsAsFactors=FALSE)

# Append tag query
Tags <- rbind(ParentId_01, ParentId_02)

rm(ParentId_01, ParentId_02)

write.csv(Tags, "./raw/Tags.csv", row.names = FALSE)
