library(dplyr)
library(survival)

# Import file
setwd("C:/Projects/Stack_Exchange/motivation_feedback/Answers/data")
data_str_tr_gt <- read.csv("data_str_tr_gt.csv", stringsAsFactors = FALSE) 
data_str_tr_gt_tags <- read.csv("data_str_tr_gt_all.csv", stringsAsFactors = FALSE)
tdm <- read.csv("tdm_all.csv", stringsAsFactors = FALSE)
silhouette_df <- read.csv("silhouette_df_all.csv", stringsAsFactors = FALSE)
cluster_df <- as.data.frame(table(data_str_tr_gt_tags$TagCluster))

# adjust for data_str_gt variables
data_str_tr_gt$weekday <- ifelse(data_str_tr_gt$day %in% c("Monday", "Tuesday", 
                                                           "Wednesday", "Thursday", 
                                                           "Friday"), 1, 0)
data_str_tr_gt$day <- NULL

data_str_tr_gt <- data_str_tr_gt[ , -which(names(data_str_tr_gt) %in% c("QuestionTag"))]

# Did the user received the autobiographer badge before answering the first question
# shows how committed users are within the platform (use as advertisement tools)

# Add badges information
Badges <- read.csv(file="./raw/Badges.csv",stringsAsFactors=FALSE)
keep <- c("UserId", "Name", "Date") 

Badges <- Badges[keep]
Badges <- subset(Badges, Name == "Autobiographer")

colnames(Badges)[3] <- "AutobiographerDate"

data_str_tr_gt <- merge(data_str_tr_gt, Badges[, c("UserId", "AutobiographerDate")], 
                      by.x = "OwnerUserId", by.y = "UserId", all.x = TRUE)

first_event <- data_str_tr_gt %>%
  filter(event == 1) %>%
  select(OwnerUserId, event, CreationDate, AutobiographerDate)

# Format date
first_event$CreationDate <- as.POSIXct(first_event$CreationDate, 
                                       format = "%Y-%m-%d %H:%M:%S", tz = "UTC")

first_event$AutobiographerDate <- as.POSIXct(first_event$AutobiographerDate, 
                                             format = "%Y-%m-%d %H:%M:%S", tz = "UTC")

# if the user completed the autobiography before answering the first question
first_event$Autobiographer <- ifelse(first_event$AutobiographerDate < 
                                       first_event$CreationDate, 1, 0 )

first_event$Autobiographer <- ifelse(is.na(first_event$Autobiographer), 0, 
                                     first_event$Autobiographer) 

data_str_tr_gt <- merge(data_str_tr_gt, first_event[, c("OwnerUserId", "Autobiographer")], 
                        by = "OwnerUserId", all.x = TRUE)

data_str_tr_gt$AutobiographerDate <- NULL

# Adjust the variables
data_str_tr_gt$year <- factor(data_str_tr_gt$year) # year the answer was asked
data_str_tr_gt$start_UX <- factor(data_str_tr_gt$start_UX) # the contributor joined UX first
# data_str_tr_gt$tenure <- factor(data_str_tr_gt$tenure) # since when the contributor registered to one community in SE
data_str_tr_gt$weekday <- factor(data_str_tr_gt$weekday) # contributor posted during weekdays or weekends
data_str_tr_gt$Autobiographer <- factor(data_str_tr_gt$Autobiographer)
data_str_tr_gt$AcceptedByOriginator <- factor(data_str_tr_gt$AcceptedByOriginator)


# gt model
model_pwp_gt_00 = coxph(Surv(tstop-tstart,status) ~
                          AcceptedByOriginator +
                          EditCount +
                          UpMod +
                          DownMod +
                          CommentCount +
                          start_UX +
                          weekday +
                          Autobiographer +
                          year +
                          tenure +
                          cluster(OwnerUserId) + strata(event), method="breslow", data=data_str_tr_gt, robust = TRUE)

summary(model_pwp_gt_00)


# Adjust the variables
data_str_tr_gt_tags$year <- factor(data_str_tr_gt_tags$year) # year the answer was asked
data_str_tr_gt_tags$start_UX <- factor(data_str_tr_gt_tags$start_UX) # the contributor joined UX first
# data_str_tr_gt_tags$tenure <- factor(data_str_tr_gt_tags$tenure) # since when the contributor registered to one community in SE
data_str_tr_gt_tags$TagCluster <- factor(data_str_tr_gt_tags$TagCluster) # interests of the contributors (grouped by similar tag)
data_str_tr_gt_tags$weekday <- factor(data_str_tr_gt_tags$weekday) # contributor posted during weekdays or weekends 
data_str_tr_gt_tags$Autobiographer <- factor(data_str_tr_gt_tags$Autobiographer)
data_str_tr_gt_tags$AcceptedByOriginator <- factor(data_str_tr_gt_tags$AcceptedByOriginator)

# gt Model with clusters
model_pwp_gt_00 = coxph(Surv(tstop-tstart,status) ~
                          AcceptedByOriginator +
                          EditCount +
                          UpMod +
                          DownMod +
                          CommentCount +
                          start_UX +
                          weekday +
                          Autobiographer +
                          year +
                          tenure +
                          TagCluster +
                          cluster(OwnerUserId) + strata(event), method="breslow", data=data_str_tr_gt_tags, robust = TRUE)

summary(model_pwp_gt_00)
