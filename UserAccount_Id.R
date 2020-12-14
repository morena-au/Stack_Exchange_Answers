## Collect USERS ACCOUNT information

library(plyr)
library(dplyr)
library(tidyr)


# Import file
setwd("C:/Projects/Stack_Exchange/motivation_feedback/Answers/data/raw")
d.ux.a.00 <- read.csv(file="./d.ux.a.00.csv", stringsAsFactors=FALSE)
keep <- c("ParentId", "Id", "CreationDate", "OwnerUserId",
          "LastActivityDate")
d.ux.a.01 <- d.ux.a.00[keep]

# Construct api call for each users

users_urls_api <- data.frame(unique(d.ux.a.01$OwnerUserId))
names(users_urls_api) <- "url"

users_urls_api$url <- paste("https://api.stackexchange.com/2.2/users/", users_urls_api$url,
                          "?site=ux.stackexchange&key=G0yd6IHl5kBtkBtsNU*4dg((", sep = "")

setwd("C:/Users/au517585/Desktop/Projects/Stack_Exchange/motivation_feedback/data/Answers")
write.csv(users_urls_api, "./users_urls_api.csv", row.names = FALSE)

# Subset for the missing calls
users_info <- read.csv(file="./users_info.csv", stringsAsFactors=FALSE)
users_urls_api <- read.csv(file="./users_urls_api.csv", stringsAsFactors = FALSE)
users_urls_api <- users_urls_api %>% extract(col = url, into = "user_id",
                                             regex = "users/(\\d+)\\?", remove = FALSE)

users_urls_api <- users_urls_api %>%
  filter(!(user_id %in% unique(users_info$user_id)))

users_urls_api$user_id <- NULL

# update file
setwd("C:/Users/au517585/Desktop/Projects/Stack_Exchange/motivation_feedback/data/Answers")
write.csv(users_urls_api, "./users_urls_api.csv", row.names = FALSE)