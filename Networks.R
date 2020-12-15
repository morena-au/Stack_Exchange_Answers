# Write down list of requests to stack exchange api to get users network account information
library(plyr)
library(dplyr)
library(tidyr)

# Import file
setwd("C:/Projects/Stack_Exchange/motivation_feedback/Answers/data/raw")
users_info <- read.csv(file="./users_info.csv", stringsAsFactors=FALSE)
users_info <- unique(users_info)

## Network id API
# Create urls for call from scrapy  to the network id api

network_urls <- data.frame(users_info[, c("account_id")])
names(network_urls) <- "url"

network_urls$url <- paste("https://api.stackexchange.com/2.2/users/", network_urls$url, 
                          "/associated?page=1&pagesize=100&key=G0yd6IHl5kBtkBtsNU*4dg((", sep = "")

write.csv(network_urls, "C:/Projects/Stack_Exchange/motivation_feedback/Answers/data/raw/urls_associated_api.csv", row.names = FALSE)


# Check what we have already collected
urls_associated_api <- read.csv(file="./urls_associated_api.csv", stringsAsFactors=FALSE)
users_associated_info <- read.csv(file="./users_associated_info.csv", stringsAsFactors=FALSE)
users_associated_info <- unique(users_associated_info)

urls_associated_api <- urls_associated_api %>% extract(col = url, into = "account_id",  
                                                       regex = "users/(\\d+)/", remove = FALSE)

urls_associated_api <- urls_associated_api %>%
  filter(!(account_id %in% unique(users_associated_info$account_id)))

urls_associated_api$account_id <- NULL

# update urls associated api with the missing requests
write.csv(urls_associated_api, "./urls_associated_api.csv", row.names = FALSE)
