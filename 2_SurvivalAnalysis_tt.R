library(dplyr)
library(survival)

# Import file
setwd("C:/Projects/Stack_Exchange/motivation_feedback/Answers/data")
data_str_tr_tt_tags <- read.csv("data_str_tr_tt_all.csv", stringsAsFactors = FALSE)
data_str_tr_tt <- read.csv("data_str_tr_tt.csv", stringsAsFactors = FALSE)
tdm <- read.csv("tdm_all.csv", stringsAsFactors = FALSE)
silhouette_df <- read.csv("silhouette_df_all.csv", stringsAsFactors = FALSE)


cluster_df <- as.data.frame(table(data_str_tr_tt_tags$TagCluster))

# adjust for data_str_tt variables
data_str_tr_tt$weekday <- ifelse(data_str_tr_tt$day %in% c("Monday", "Tuesday", 
                                                           "Wednesday", "Thursday", 
                                                           "Friday"), 1, 0)
data_str_tr_tt$day <- NULL

data_str_tr_tt <- data_str_tr_tt[ , -which(names(data_str_tr_tt) %in% c("TagName","TagFreq"))]

# Did the user received the autobiographer badge before answering the first question
# shows how committed users are within the platform (use as advertisement tools)

first_event <- data_str_tr_tt %>%
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

data_str_tr_tt <- merge(data_str_tr_tt, first_event[, c("OwnerUserId", "Autobiographer")], 
                        by = "OwnerUserId", all.x = TRUE)

data_str_tr_tt$AutobiographerDate <- NULL

# Adjust the variables
data_str_tr_tt$year <- factor(data_str_tr_tt$year) # year the answer was asked
data_str_tr_tt$start_UX <- factor(data_str_tr_tt$start_UX) # the contributor joined UX first
# data_str_tr_tt$tenure <- factor(data_str_tr_tt$tenure) # since when the contributor registered to one community in SE
data_str_tr_tt$weekday <- factor(data_str_tr_tt$weekday) # contributor posted during weekdays or weekends
data_str_tr_tt$Autobiographer <- factor(data_str_tr_tt$Autobiographer)

# tt Model with clusters
model_pwp_tt_00 = coxph(Surv(tstart, tstop, status) ~
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
                          cluster(OwnerUserId) + strata(event), method="breslow", data=data_str_tr_tt, robust = TRUE)

summary(model_pwp_tt_00)


# Adjust the variables
data_str_tr_tt_tags$year <- factor(data_str_tr_tt_tags$year) # year the answer was asked
data_str_tr_tt_tags$start_UX <- factor(data_str_tr_tt_tags$start_UX) # the contributor joined UX first
# data_str_tr_tt_tags$tenure <- factor(data_str_tr_tt_tags$tenure) # since when the contributor registered to one community in SE
data_str_tr_tt_tags$TagCluster <- factor(data_str_tr_tt_tags$TagCluster) # interests of the contributors (grouped by similar tag)
data_str_tr_tt_tags$weekday <- factor(data_str_tr_tt_tags$weekday) # contributor posted during weekdays or weekends
data_str_tr_tt_tags$Autobiographer <- factor(data_str_tr_tt_tags$Autobiographer)

# tt Model with clusters
model_pwp_tt_01 = coxph(Surv(tstart, tstop, status) ~
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
                          cluster(OwnerUserId) + strata(event), method="breslow", data=data_str_tr_tt_tags, robust = TRUE)

summary(model_pwp_tt_01)

