library(dplyr)
library(survival)

# Import file
setwd("C:/Projects/Stack_Exchange/motivation_feedback/Answers/data")
data_str_tr_tt <- read.csv("data_str_tr_tt_00_07.csv", stringsAsFactors = FALSE)
data_str_tr_gt <- read.csv("data_str_tr_gt_00_07.csv", stringsAsFactors = FALSE) 

# remove QuestionTag and add AutobiographerDate
data_str_tr_gt <- subset(data_str_tr_gt, select = -c(QuestionTag))

Autobiographer_df <- data_str_tr_tt[, c("OwnerUserId",
                                        "AutobiographerDate")]

Autobiographer_df <- subset(Autobiographer_df, !(is.na(AutobiographerDate)))
Autobiographer_df <- Autobiographer_df[!duplicated(Autobiographer_df), ]


data_str_tr_gt <- merge(data_str_tr_gt, Autobiographer_df, 
                        by = "OwnerUserId", all.x = TRUE)
rm(Autobiographer_df)

# Create a weekday and a weekend variable
data_str_tr_gt$weekday <- ifelse(data_str_tr_gt$day %in% c("Monday", "Tuesday", 
                                                           "Wednesday", "Thursday", 
                                                           "Friday"), 1, 0)

# Make sure that the tag cluster have at least one different
# treatment manifestation within the cluster

TagCluster_gt <- data_str_tr_gt %>%
  group_by(TagCluster) %>%
  summarise(UniqueEdit = length(unique(EditCount)),
         UniqueAcc = length(unique(AcceptedByOriginator)),
         UniqueUp = length(unique(UpMod)),
         UniqueMod = length(unique(DownMod)),
         UniqueComm = length(unique(CommentCount))) %>%
  select(TagCluster, UniqueEdit, UniqueAcc, 
         UniqueUp, UniqueMod, UniqueComm)

TagCluster_gt <- subset(TagCluster_gt, !(UniqueEdit == 1 & UniqueAcc == 1 & UniqueUp == 1 &
         UniqueMod == 1 & UniqueComm == 1))

# Subset gt by the clusters who have at least one different treatment
data_str_tr_gt <- subset(data_str_tr_gt, TagCluster %in% TagCluster_gt$TagCluster)

# Adjust the variables
data_str_tr_gt$year <- factor(data_str_tr_gt$year) # year the answer was asked
data_str_tr_gt$start_UX <- factor(data_str_tr_gt$start_UX) # the contributor joined UX first
data_str_tr_gt$tenure <- factor(data_str_tr_gt$tenure) # since when the contributor registered to one community in SE
data_str_tr_gt$TagCluster <- factor(data_str_tr_gt$TagCluster) # interests of the contributors (grouped by similar tag)
data_str_tr_gt$weekday <- factor(data_str_tr_gt$weekday) # contributor posted during weekdays or weekends 

# Did the user received the autobiographer badge before answering the first question
# shows how committed users are within the platform (use as advertisement tools)

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

data_str_tr_gt$Autobiographer <- factor(data_str_tr_gt$Autobiographer)
data_str_tr_gt$AcceptedByOriginator <- factor(data_str_tr_gt$AcceptedByOriginator)
data_str_tr_gt$score <- data_str_tr_gt$UpMod - data_str_tr_gt$DownMod

# gt model
model_pwp_gt_00 = coxph(Surv(tstop-tstart,status) ~
                          AcceptedByOriginator + 
                          EditCount +
                          # score +
                          UpMod +
                          DownMod +
                          CommentCount + 
                          start_UX +
                          weekday +
                          Autobiographer +
                          year +
                          tenure +
                          TagCluster +
                          cluster(OwnerUserId) + strata(event), method="breslow", data=data_str_tr_gt, robust = TRUE)

summary(model_pwp_gt_00)

