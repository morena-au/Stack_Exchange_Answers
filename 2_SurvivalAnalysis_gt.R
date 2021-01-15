library(dplyr)
library(survival)

# Import file
setwd("C:/Projects/Stack_Exchange/motivation_feedback/Answers/data")
data_str_tr_gt <- read.csv("data_str_tr_gt.csv", stringsAsFactors = FALSE) 
data_str_tr_gt_tags <- read.csv("data_str_tr_gt_all.csv", stringsAsFactors = FALSE)
tdm <- read.csv("tdm_all.csv", stringsAsFactors = FALSE)
silhouette_df <- read.csv("silhouette_df_all.csv", stringsAsFactors = FALSE)
cluster_df <- as.data.frame(table(data_str_tr_gt_tags$TagCluster))

# write reputation urls calls 
reputation <- as.data.frame(paste0('https://ux.stackexchange.com/users/', unique(data_str_tr_gt$OwnerUserId),
                     '?tab=reputation&sort=post&page=1'))
colnames(reputation) <- 'url'
setwd("C:/Projects/Stack_Exchange/motivation_feedback/Answers/data/old/Reputation")
write.csv(reputation, "./reputation.csv", row.names = FALSE)

# # adjust for data_str_gt variables
# data_str_tr_gt$weekday <- ifelse(data_str_tr_gt$day %in% c("Monday", "Tuesday", 
#                                                            "Wednesday", "Thursday", 
#                                                            "Friday"), 1, 0)
# data_str_tr_gt$day <- NULL
# 
# data_str_tr_gt <- data_str_tr_gt[ , -which(names(data_str_tr_gt) %in% c("QuestionTag"))]
# 
# # Did the user received the autobiographer badge before answering the first question
# # shows how committed users are within the platform (use as advertisement tools)
# 
# # Add badges information
# Badges <- read.csv(file="./raw/Badges.csv",stringsAsFactors=FALSE)
# keep <- c("UserId", "Name", "Date") 
# 
# Badges <- Badges[keep]
# Badges <- subset(Badges, Name == "Autobiographer")
# 
# colnames(Badges)[3] <- "AutobiographerDate"
# 
# data_str_tr_gt <- merge(data_str_tr_gt, Badges[, c("UserId", "AutobiographerDate")], 
#                       by.x = "OwnerUserId", by.y = "UserId", all.x = TRUE)
# 
# first_event <- data_str_tr_gt %>%
#   filter(event == 1) %>%
#   select(OwnerUserId, event, CreationDate, AutobiographerDate)
# 
# # Format date
# first_event$CreationDate <- as.POSIXct(first_event$CreationDate, 
#                                        format = "%Y-%m-%d %H:%M:%S", tz = "UTC")
# 
# first_event$AutobiographerDate <- as.POSIXct(first_event$AutobiographerDate, 
#                                              format = "%Y-%m-%d %H:%M:%S", tz = "UTC")
# 
# # if the user completed the autobiography before answering the first question
# first_event$Autobiographer <- ifelse(first_event$AutobiographerDate < 
#                                        first_event$CreationDate, 1, 0 )
# 
# first_event$Autobiographer <- ifelse(is.na(first_event$Autobiographer), 0, 
#                                      first_event$Autobiographer) 
# 
# data_str_tr_gt <- merge(data_str_tr_gt, first_event[, c("OwnerUserId", "Autobiographer")], 
#                         by = "OwnerUserId", all.x = TRUE)
# 
# data_str_tr_gt$AutobiographerDate <- NULL
# 
# # Save the file
# write.csv(data_str_tr_gt, "data_str_tr_gt.csv", row.names = FALSE)

# add reputation 
setwd("C:/Projects/Stack_Exchange/motivation_feedback/Answers/data/raw")
reputation <- read.csv("Reputation.csv", stringsAsFactors = FALSE) 

# time formatting
reputation$time_UTC <- as.POSIXct(substr(reputation$time_UTC,1,nchar(reputation$time_UTC)-1), 
                                         format = "%Y-%m-%d %H:%M:%S", tz = "UTC")

# Calculate totRep until CreationTime
reputation_df <- merge(data_str_tr_gt[,c("OwnerUserId", "Id", "CreationDate")], reputation, 
      by = "OwnerUserId", all.x = TRUE)
    
      
reputation_sum <- reputation_df %>%
  group_by(Id) %>%
  summarize(TotRep = sum(reputation[time_UTC <= CreationDate]))

# add 1 reputation points to all. 
# 1 point is given as default when you register 
reputation_sum$TotRep <- ifelse(is.na(reputation_sum$TotRep), 0, reputation_sum$TotRep)
reputation_sum$TotRep <- reputation_sum$TotRep + 1

# Merge
data_str_tr_gt <- merge(data_str_tr_gt, reputation_sum, 
                        by = "Id", all.x = TRUE)


# Adjust the variables
data_str_tr_gt$year <- factor(data_str_tr_gt$year) # year the answer was asked
data_str_tr_gt$start_UX <- factor(data_str_tr_gt$start_UX) # the contributor joined UX first
# data_str_tr_gt$tenure <- factor(data_str_tr_gt$tenure) # since when the contributor registered to one community in SE
data_str_tr_gt$weekday <- factor(data_str_tr_gt$weekday) # contributor posted during weekdays or weekends
data_str_tr_gt$Autobiographer <- factor(data_str_tr_gt$Autobiographer)
data_str_tr_gt$AcceptedByOriginator <- factor(data_str_tr_gt$AcceptedByOriginator)

# Create tenure within the UX and tenure with SE (year when first join SE independently 
# from the community)

data_str_tr_gt$tenure_UX <- substring(data_str_tr_gt$UX_registration, 1, 4)
data_str_tr_gt$tenure_UX <- as.numeric(data_str_tr_gt$tenure_UX)

data_str_tr_gt$tenure_SE <- substring(data_str_tr_gt$SE_registration, 1, 4)
data_str_tr_gt$tenure_SE <- as.numeric(data_str_tr_gt$tenure_SE)

# DESCRIPTIVE 

# Start UX
#Base Information
for (i in unique(data_str_tr_gt$event)) {
  print(subset(data_str_tr_gt, event == i) %>%
          group_by(start_UX) %>%
          tally())
}

# Time count of censured across accepted answers
for (i in unique(data_str_tr_gt$event)) {
  event <- subset(data_str_tr_gt, event == i) %>%
    group_by(start_UX, status) %>%
    tally()
  
  # Calculate the totals by column
  total <- aggregate(event$n, by=list(Status = event$status), FUN = sum)
  total <- matrix(c(total$x), nrow = 1, ncol = 2, byrow = TRUE)
  
  if (length(event$n) == 2) {
    event <- matrix(c(event$n, 0, 0), nrow = 2, ncol = 2, byrow = TRUE)
  } else {
    event <- matrix(c(event$n), nrow = 2, ncol = 2, byrow = TRUE)
  }
  
  print(paste("EVENT ", i))
  print(event)
  
  event_prop <- matrix(c(0, 0, 0, 0), nrow = 2, ncol = 2, byrow = TRUE)
  
  event_prop[,1] <- round((event[,1]/total[,1])*100, 2)
  event_prop[,2] <- round((event[,2]/total[,2])*100, 2)
  
  print("Observed Proportions")
  print(event_prop)
  
  
  print("Chi-squared Test")
  print(chisq.test(event))
  
  print("#################################")
}


# Autobiographer
#Base Information
for (i in unique(data_str_tr_gt$event)) {
  print(subset(data_str_tr_gt, event == i) %>%
          group_by(Autobiographer) %>%
          tally())
}

# Time count of censured across accepted answers
for (i in unique(data_str_tr_gt$event)) {
  event <- subset(data_str_tr_gt, event == i) %>%
    group_by(Autobiographer, status) %>%
    tally()
  
  # Calculate the totals by column
  total <- aggregate(event$n, by=list(Status = event$status), FUN = sum)
  total <- matrix(c(total$x), nrow = 1, ncol = 2, byrow = TRUE)
  
  if (length(event$n) == 2) {
    event <- matrix(c(event$n, 0, 0), nrow = 2, ncol = 2, byrow = TRUE)
  } else {
    event <- matrix(c(event$n), nrow = 2, ncol = 2, byrow = TRUE)
  }
  
  print(paste("EVENT ", i))
  print(event)
  
  event_prop <- matrix(c(0, 0, 0, 0), nrow = 2, ncol = 2, byrow = TRUE)
  
  event_prop[,1] <- round((event[,1]/total[,1])*100, 2)
  event_prop[,2] <- round((event[,2]/total[,2])*100, 2)
  
  print("Observed Proportions")
  print(event_prop)
  
  
  print("Chi-squared Test")
  print(chisq.test(event))
  
  print("#################################")
}



# year
#Base Information
for (i in unique(data_str_tr_gt$event)) {
  print(subset(data_str_tr_gt, event == i) %>%
          group_by(year) %>%
          tally())
}

# Time count of censured across accepted answers
for (i in unique(data_str_tr_gt$event)) {
  event <- subset(data_str_tr_gt, event == i) %>%
    group_by(year, status) %>%
    tally()
  
  # Calculate the totals by column
  total <- aggregate(event$n, by=list(Status = event$status), FUN = sum)
  total <- matrix(c(total$x), nrow = 1, ncol = 2, byrow = TRUE)
  
  event <- matrix(c(event$n), nrow = 7, ncol = 2, byrow = TRUE)
  
  print(paste("EVENT ", i))
  print(event)
  
  event_prop <- matrix(rep(0, 7), nrow = 7, ncol = 2, byrow = TRUE)
  
  event_prop[,1] <- round((event[,1]/total[,1])*100, 2)
  event_prop[,2] <- round((event[,2]/total[,2])*100, 2)
  
  print("Observed Proportions")
  print(event_prop)
  
  
  print("Chi-squared Test")
  print(chisq.test(event))
  
  print("#################################")
}

# tenure
for (i in unique(data_str_tr_gt$event)) {
  print(subset(data_str_tr_gt, event == i) %>%
          summarise(mean = mean(tenure), 
                    median = median(tenure),
                    sdt = sd(tenure)))
}

# Numerical descriptive table 
for (i in unique(data_str_tr_gt$event)) {
  print(paste("Event", i))
  print(subset(data_str_tr_gt, event == i) %>%
          group_by(status)%>%
          summarise(mean(tenure), median(tenure),sd(tenure)))
  print("===============================================")
}

for (i in unique(data_str_tr_gt$event)) {
  print(paste("Event", i))
  event <- subset(data_str_tr_gt, event == i)
  print(pairwise.wilcox.test(event$tenure, event$status, p.adjust.method="none"))
  #print(pairwise.wilcox.test(event$EditCount, event$status, exact = FALSE))
  print("===============================================")
}


# weekday
#Base Information
for (i in 1:4) {
  print(subset(data_str_tr_gt, event == i) %>%
          group_by(weekday) %>%
          tally())
}

# Time count of censured across accepted answers
for (i in 1:4) {
  event <- subset(data_str_tr_gt, event == i) %>%
    group_by(weekday, status) %>%
    tally()
  
  # Calculate the totals by column
  total <- aggregate(event$n, by=list(Status = event$status), FUN = sum)
  total <- matrix(c(total$x), nrow = 1, ncol = 2, byrow = TRUE)
  
  if (length(event$n) == 2) {
    event <- matrix(c(event$n, 0, 0), nrow = 2, ncol = 2, byrow = TRUE)
  } else {
    event <- matrix(c(event$n), nrow = 2, ncol = 2, byrow = TRUE)
  }
  
  print(paste("EVENT ", i))
  print(event)
  
  event_prop <- matrix(c(0, 0, 0, 0), nrow = 2, ncol = 2, byrow = TRUE)
  
  event_prop[,1] <- round((event[,1]/total[,1])*100, 2)
  event_prop[,2] <- round((event[,2]/total[,2])*100, 2)
  
  print("Observed Proportions")
  print(event_prop)
  
  
  print("Chi-squared Test")
  print(chisq.test(event))
  
  print("#################################")
}


# gt model
model_pwp_gt_00 = coxph(Surv(tstop-tstart,status) ~
                          UpMod +
                          DownMod +
                          AcceptedByOriginator +
                          EditCount +
                          CommentCount +
                          start_UX +
                          Autobiographer +
                          year +
                          tenure_UX +
                          tenure_SE +
                          weekday +
                          TotRep +
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

# Tags Descriptive 
table(data_str_tr_gt_tags$status, data_str_tr_gt_tags$event)

# Accepted by the originator
for (i in unique(data_str_tr_gt_tags$event)) {
  print(subset(data_str_tr_gt_tags, event == i) %>%
          group_by(AcceptedByOriginator) %>%
          tally())
}


# Time count of censured across accepted answers
for (i in unique(data_str_tr_gt_tags$event)) {
  event <- subset(data_str_tr_gt_tags, event == i) %>%
    group_by(AcceptedByOriginator, status) %>%
    tally()
  
  # Calculate the totals by column
  total <- aggregate(event$n, by=list(Status = event$status), FUN = sum)
  total <- matrix(c(total$x), nrow = 1, ncol = 2, byrow = TRUE)
  
  if (length(event$n) == 2) {
    event <- matrix(c(event$n, 0, 0), nrow = 2, ncol = 2, byrow = TRUE)
  } else {
    event <- matrix(c(event$n), nrow = 2, ncol = 2, byrow = TRUE)
  }
  
  print(paste("EVENT ", i))
  print(event)
  
  event_prop <- matrix(c(0, 0, 0, 0), nrow = 2, ncol = 2, byrow = TRUE)
  
  event_prop[,1] <- round((event[,1]/total[,1])*100, 2)
  event_prop[,2] <- round((event[,2]/total[,2])*100, 2)
  
  print("Observed Proportions")
  print(event_prop)
  
  
  print("Chi-squared Test")
  if (i != 1) { # because it is not possible to calculate it 
    print(chisq.test(event))
  }
  
  print("#################################")
}

# - "EditCount" 
# Base Information table

for (i in unique(data_str_tr_gt_tags$event)) {
  print(subset(data_str_tr_gt_tags, event == i) %>%
          tally(EditCount))
}

# Numerical descriptive table 
for (i in unique(data_str_tr_gt_tags$event)) {
  print(paste("Event", i))
  print(subset(data_str_tr_gt_tags, event == i) %>%
          group_by(status)%>%
          summarise(round(mean(EditCount),3), median(EditCount),sd(EditCount)))
  print("===============================================")
}

for (i in unique(data_str_tr_gt_tags$event)) {
  print(paste("Event", i))
  event <- subset(data_str_tr_gt_tags, event == i)
  print(pairwise.wilcox.test(event$EditCount, event$status, p.adjust.method="none"))
  #print(pairwise.wilcox.test(event$EditCount, event$status, exact = FALSE))
  print("===============================================")
}


# - "UpMod"  

for (i in unique(data_str_tr_gt_tags$event)) {
  print(subset(data_str_tr_gt_tags, event == i) %>%
          tally(UpMod))
}


for (i in unique(data_str_tr_gt_tags$event)) {
  print(paste("Event", i))
  print(subset(data_str_tr_gt_tags, event == i) %>%
          group_by(status)%>%
          summarise(round(mean(UpMod), 3), median(UpMod),sd(UpMod)))
  print("===============================================")
}

for (i in unique(data_str_tr_gt_tags$event)) {
  print(paste("Event", i))
  event <- subset(data_str_tr_gt_tags, event == i)
  print(pairwise.wilcox.test(event$UpMod, event$status, p.adjust.method="none"))
  # print(pairwise.wilcox.test(event$UpMod, event$status, exact = FALSE))
  print("===============================================")
}


# - "DownMod" 

for (i in unique(data_str_tr_gt_tags$event)) {
  print(subset(data_str_tr_gt_tags, event == i) %>%
          tally(DownMod))
}


for (i in unique(data_str_tr_gt_tags$event)) {
  print(paste("Event", i))
  print(subset(data_str_tr_gt_tags, event == i) %>%
          group_by(status)%>%
          summarise(round(mean(DownMod), 3), median(DownMod),sd(DownMod)))
  print("===============================================")
}

for (i in unique(data_str_tr_gt_tags$event)) {
  print(paste("Event", i))
  event <- subset(data_str_tr_gt_tags, event == i)
  print(pairwise.wilcox.test(event$DownMod, event$status, p.adjust.method="none"))
  # print(pairwise.wilcox.test(event$DownMod, event$status, exact = FALSE))
  print("===============================================")
}

# - "CommentCount"

for (i in unique(data_str_tr_gt_tags$event)) {
  print(subset(data_str_tr_gt_tags, event == i) %>%
          tally(CommentCount))
}


for (i in unique(data_str_tr_gt_tags$event)) {
  print(paste("Event", i))
  print(subset(data_str_tr_gt_tags, event == i) %>%
          group_by(status)%>%
          summarise(round(mean(CommentCount), 3), median(CommentCount),sd(CommentCount)))
  print("===============================================")
}

for (i in unique(data_str_tr_gt_tags$event)) {
  print(paste("Event", i))
  event <- subset(data_str_tr_gt_tags, event == i)
  print(pairwise.wilcox.test(event$CommentCount, event$status, p.adjust.method="none"))
  # print(pairwise.wilcox.test(event$CommentCount, event$status, exact = FALSE))
  print("===============================================")
}


# Start UX
#Base Information
for (i in unique(data_str_tr_gt_tags$event)) {
  print(subset(data_str_tr_gt_tags, event == i) %>%
          group_by(start_UX) %>%
          tally())
}

# Time count of censured across accepted answers
for (i in unique(data_str_tr_gt_tags$event)) {
  event <- subset(data_str_tr_gt_tags, event == i) %>%
    group_by(start_UX, status) %>%
    tally()
  
  # Calculate the totals by column
  total <- aggregate(event$n, by=list(Status = event$status), FUN = sum)
  total <- matrix(c(total$x), nrow = 1, ncol = 2, byrow = TRUE)
  
  if (length(event$n) == 2) {
    event <- matrix(c(event$n, 0, 0), nrow = 2, ncol = 2, byrow = TRUE)
  } else {
    event <- matrix(c(event$n), nrow = 2, ncol = 2, byrow = TRUE)
  }
  
  print(paste("EVENT ", i))
  print(event)
  
  event_prop <- matrix(c(0, 0, 0, 0), nrow = 2, ncol = 2, byrow = TRUE)
  
  event_prop[,1] <- round((event[,1]/total[,1])*100, 2)
  event_prop[,2] <- round((event[,2]/total[,2])*100, 2)
  
  print("Observed Proportions")
  print(event_prop)
  
  
  print("Chi-squared Test")
  print(chisq.test(event))
  
  print("#################################")
}


# Autobiographer
#Base Information
for (i in unique(data_str_tr_gt_tags$event)) {
  print(subset(data_str_tr_gt_tags, event == i) %>%
          group_by(Autobiographer) %>%
          tally())
}

# Time count of censured across accepted answers
for (i in unique(data_str_tr_gt_tags$event)) {
  event <- subset(data_str_tr_gt_tags, event == i) %>%
    group_by(Autobiographer, status) %>%
    tally()
  
  # Calculate the totals by column
  total <- aggregate(event$n, by=list(Status = event$status), FUN = sum)
  total <- matrix(c(total$x), nrow = 1, ncol = 2, byrow = TRUE)
  
  if (length(event$n) == 2) {
    event <- matrix(c(event$n, 0, 0), nrow = 2, ncol = 2, byrow = TRUE)
  } else {
    event <- matrix(c(event$n), nrow = 2, ncol = 2, byrow = TRUE)
  }
  
  print(paste("EVENT ", i))
  print(event)
  
  event_prop <- matrix(c(0, 0, 0, 0), nrow = 2, ncol = 2, byrow = TRUE)
  
  event_prop[,1] <- round((event[,1]/total[,1])*100, 2)
  event_prop[,2] <- round((event[,2]/total[,2])*100, 2)
  
  print("Observed Proportions")
  print(event_prop)
  
  
  print("Chi-squared Test")
  print(chisq.test(event))
  
  print("#################################")
}

# year
#Base Information
for (i in unique(data_str_tr_gt_tags$event)) {
  print(subset(data_str_tr_gt_tags, event == i) %>%
          group_by(year) %>%
          tally())
}

# Time count of censured across accepted answers
for (i in unique(data_str_tr_gt_tags$event)) {
  event <- subset(data_str_tr_gt_tags, event == i) %>%
    group_by(year, status) %>%
    tally()
  
  # Calculate the totals by column
  total <- aggregate(event$n, by=list(Status = event$status), FUN = sum)
  total <- matrix(c(total$x), nrow = 1, ncol = 2, byrow = TRUE)
  
  print("TOTAL")
  event <- matrix(c(event$n), nrow = 7, ncol = 2, byrow = TRUE)
  
  print(paste("EVENT ", i))
  print(event)
  
  event_prop <- matrix(rep(0, 7), nrow = 7, ncol = 2, byrow = TRUE)
  
  event_prop[,1] <- round((event[,1]/total[,1])*100, 2)
  event_prop[,2] <- round((event[,2]/total[,2])*100, 2)
  
  print("Observed Proportions")
  print(event_prop)
  
  
  print("Chi-squared Test")
  print(chisq.test(event)) # Warning due to the low number of observations (reduce at 3 event maybe?)
  print("Chi-squared with simulation conditional on the marginals")
  print(chisq.test(event, simulate.p.value = TRUE))
  
  print("#################################")
}


#tenure
for (i in unique(data_str_tr_gt_tags$event)) {
  print(subset(data_str_tr_gt_tags, event == i) %>%
          summarise(mean = mean(tenure), 
                    median = median(tenure),
                    sdt = sd(tenure)))
}

# Numerical descriptive table 
for (i in unique(data_str_tr_gt_tags$event)) {
  print(paste("Event", i))
  print(subset(data_str_tr_gt_tags, event == i) %>%
          group_by(status)%>%
          summarise(mean(tenure), median(tenure),sd(tenure)))
  print("===============================================")
}

for (i in unique(data_str_tr_gt_tags$event)) {
  print(paste("Event", i))
  event <- subset(data_str_tr_gt_tags, event == i)
  print(pairwise.wilcox.test(event$tenure, event$status, p.adjust.method="none"))
  #print(pairwise.wilcox.test(event$EditCount, event$status, exact = FALSE))
  print("===============================================")
}


# weekday
#Base Information
for (i in 1:4) {
  print(subset(data_str_tr_gt_tags, event == i) %>%
          group_by(weekday) %>%
          tally())
}

# Time count of censured across accepted answers
for (i in 1:4) {
  event <- subset(data_str_tr_gt_tags, event == i) %>%
    group_by(weekday, status) %>%
    tally()
  
  # Calculate the totals by column
  total <- aggregate(event$n, by=list(Status = event$status), FUN = sum)
  total <- matrix(c(total$x), nrow = 1, ncol = 2, byrow = TRUE)
  
  if (length(event$n) == 2) {
    event <- matrix(c(event$n, 0, 0), nrow = 2, ncol = 2, byrow = TRUE)
  } else {
    event <- matrix(c(event$n), nrow = 2, ncol = 2, byrow = TRUE)
  }
  
  print(paste("EVENT ", i))
  print(event)
  
  event_prop <- matrix(c(0, 0, 0, 0), nrow = 2, ncol = 2, byrow = TRUE)
  
  event_prop[,1] <- round((event[,1]/total[,1])*100, 2)
  event_prop[,2] <- round((event[,2]/total[,2])*100, 2)
  
  print("Observed Proportions")
  print(event_prop)
  
  
  print("Chi-squared Test")
  print(chisq.test(event))
  
  print("#################################")
}

# gt Model with clusters
model_pwp_gt_00 = coxph(Surv(tstop-tstart,status) ~
                          AcceptedByOriginator +
                          EditCount +
                          UpMod +
                          DownMod +
                          CommentCount +
                          start_UX +
                          weekday +
                          # Autobiographer +
                          # year +
                          # tenure +
                          # TagCluster +
                          cluster(OwnerUserId) + strata(event), method="breslow", data=data_str_tr_gt_tags, robust = TRUE)

summary(model_pwp_gt_00)
