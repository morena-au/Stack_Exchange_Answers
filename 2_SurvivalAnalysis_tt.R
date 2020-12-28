library(dplyr)
library(survival)

# Import file
setwd("C:/Projects/Stack_Exchange/motivation_feedback/Answers/data")
data_str_tr_tt <- read.csv("data_str_tr_tt_00_05.csv", stringsAsFactors = FALSE)

data_str_tr_tt$weekday <- ifelse(data_str_tr_tt$day %in% c("Monday", "Tuesday", 
                                                           "Wednesday", "Thursday", 
                                                           "Friday"), 1, 0)

# Make sure that the tag cluster have at least one different
# treatment manifestation within the cluster

TagCluster_tt <- data_str_tr_tt %>%
  group_by(TagCluster) %>%
  summarise(UniqueEdit = length(unique(EditCount)),
            UniqueAcc = length(unique(AcceptedByOriginator)),
            UniqueUp = length(unique(UpMod)),
            UniqueMod = length(unique(DownMod)),
            UniqueComm = length(unique(CommentCount))) %>%
  select(TagCluster, UniqueEdit, UniqueAcc, 
         UniqueUp, UniqueMod, UniqueComm)

TagCluster_tt <- subset(TagCluster_tt, !(UniqueEdit == 1 & UniqueAcc == 1 & UniqueUp == 1 &
                                           UniqueMod == 1 & UniqueComm == 1))

# Subset gt by the clusters who have at least one different treatment
data_str_tr_tt <- subset(data_str_tr_tt, TagCluster %in% TagCluster_tt$TagCluster)

# Adjust the variables
data_str_tr_tt$year <- factor(data_str_tr_tt$year) # year the answer was asked
data_str_tr_tt$start_UX <- factor(data_str_tr_tt$start_UX) # the contributor joined UX first
data_str_tr_tt$tenure <- factor(data_str_tr_tt$tenure) # since when the contributor registered to one community in SE
data_str_tr_tt$TagCluster <- factor(data_str_tr_tt$TagCluster) # interests of the contributors (grouped by similar tag)
data_str_tr_tt$weekday <- factor(data_str_tr_tt$weekday) # contributor posted during weekdays or weekends 

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

data_str_tr_tt$Autobiographer <- factor(data_str_tr_tt$Autobiographer)
data_str_tr_tt$score <- data_str_tr_tt$UpMod - data_str_tr_tt$DownMod

# tt Model
model_pwp_tt_00 = coxph(Surv(tstart, tstop, status) ~
                          AcceptedByOriginator + 
                          EditCount +
                          # score +
                          UpMod +
                          DownMod +
                          CommentCount + 
                          year +
                          tenure +
                          start_UX +
                          TagCluster +
                          weekday +
                          Autobiographer +
                          cluster(OwnerUserId) + strata(event), method="breslow", data=data_str_tr_tt, robust = TRUE)

summary(model_pwp_tt_00)

