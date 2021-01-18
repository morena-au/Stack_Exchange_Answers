library(stringr)

# add reputation 
setwd("C:/Projects/Stack_Exchange/motivation_feedback/Answers/data/raw")
reputation0_3000 <- read.csv("Reputation.csv", stringsAsFactors = FALSE) 
reputation3000_6000 <- read.csv("Reputation3000_6000.csv", stringsAsFactors = FALSE) 
reputation6000_9025 <- read.csv("Reputation6000_9025.csv", stringsAsFactors = FALSE) 

reputation <- rbind(reputation0_3000, reputation3000_6000, reputation6000_9025)
reputation <- reputation[!duplicated(reputation), ]

rm(reputation0_3000, reputation3000_6000, reputation6000_9025)
# check if the missing users have no reputation change
# or if we did not scrape them

setwd("C:/Projects/Stack_Exchange/motivation_feedback/Answers/data/old")
urls <- read.csv("Reputation/reputation.csv", stringsAsFactors = FALSE)

urls$OwnerUserId <- str_match(urls$url, "users/(\\d+)\\?")[, 2]

urls <- subset(urls, !(OwnerUserId %in% reputation$OwnerUserId))

urls$OwnerUserId <- NULL

# write it down and check if they really have no reputation changes
setwd('C:/Projects/Stack_Exchange/motivation_feedback/Answers/data/old/Reputation')
write.csv(urls, "NOreputation_urls.csv", row.names = FALSE)


# Final check that missing users have all no reputations changes
setwd("C:/Projects/Stack_Exchange/motivation_feedback/Answers/data/raw")
NO_reputation <- read.csv("NOReputation.csv", stringsAsFactors = FALSE)

# write down final
setwd("C:/Projects/Stack_Exchange/motivation_feedback/Answers/data/raw")
write.csv(reputation, "Reputation.csv", row.names = FALSE)
