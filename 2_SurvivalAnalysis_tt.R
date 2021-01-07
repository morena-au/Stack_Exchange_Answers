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

# Tags Descriptive 
table(data_str_tr_tt_tags$status, data_str_tr_tt_tags$event)

# Accepted by the originator
for (i in 1:4) {
  print(subset(data_str_tr_tt_tags, event == i) %>%
          tally(AcceptedByOriginator))
}


# Numerical descriptive table 
for (i in 1:4) {
  print(paste("Event", i))
  print(subset(data_str_tr_tt_tags, event == i) %>%
          group_by(status)%>%
          summarise(round(mean(AcceptedByOriginator),3), median(AcceptedByOriginator),sd(AcceptedByOriginator)))
  print("===============================================")
}

for (i in 1:4) {
  print(paste("Event", i))
  event <- subset(data_str_tr_tt_tags, event == i)
  print(pairwise.wilcox.test(event$AcceptedByOriginator, event$status, p.adjust.method="none"))
  #print(pairwise.wilcox.test(event$AcceptedByOriginator, event$status, exact = FALSE))
  print("===============================================")
}


# - "EditCount" 
# Base Information table

for (i in 1:4) {
  print(subset(data_str_tr_tt_tags, event == i) %>%
          tally(EditCount))
}

# Numerical descriptive table 
for (i in 1:4) {
  print(paste("Event", i))
  print(subset(data_str_tr_tt_tags, event == i) %>%
          group_by(status)%>%
          summarise(round(mean(EditCount),3), median(EditCount),sd(EditCount)))
  print("===============================================")
}

for (i in 1:4) {
  print(paste("Event", i))
  event <- subset(data_str_tr_tt_tags, event == i)
  print(pairwise.wilcox.test(event$EditCount, event$status, p.adjust.method="none"))
  #print(pairwise.wilcox.test(event$EditCount, event$status, exact = FALSE))
  print("===============================================")
}


# - "UpMod"  

for (i in 1:4) {
  print(subset(data_str_tr_tt_tags, event == i) %>%
          tally(UpMod))
}


for (i in 1:4) {
  print(paste("Event", i))
  print(subset(data_str_tr_tt_tags, event == i) %>%
          group_by(status)%>%
          summarise(round(mean(UpMod), 3), median(UpMod),sd(UpMod)))
  print("===============================================")
}

for (i in 1:4) {
  print(paste("Event", i))
  event <- subset(data_str_tr_tt_tags, event == i)
  print(pairwise.wilcox.test(event$UpMod, event$status, p.adjust.method="none"))
  # print(pairwise.wilcox.test(event$UpMod, event$status, exact = FALSE))
  print("===============================================")
}


# - "DownMod" 

for (i in 1:4) {
  print(subset(data_str_tr_tt_tags, event == i) %>%
          tally(DownMod))
}


for (i in 1:4) {
  print(paste("Event", i))
  print(subset(data_str_tr_tt_tags, event == i) %>%
          group_by(status)%>%
          summarise(round(mean(DownMod), 3), median(DownMod),sd(DownMod)))
  print("===============================================")
}

for (i in 1:4) {
  print(paste("Event", i))
  event <- subset(data_str_tr_tt_tags, event == i)
  print(pairwise.wilcox.test(event$DownMod, event$status, p.adjust.method="none"))
  # print(pairwise.wilcox.test(event$DownMod, event$status, exact = FALSE))
  print("===============================================")
}

# - "CommentCount"

for (i in 1:4) {
  print(subset(data_str_tr_tt_tags, event == i) %>%
          tally(CommentCount))
}


for (i in 1:4) {
  print(paste("Event", i))
  print(subset(data_str_tr_tt_tags, event == i) %>%
          group_by(status)%>%
          summarise(round(mean(CommentCount), 3), median(CommentCount),sd(CommentCount)))
  print("===============================================")
}

for (i in 1:4) {
  print(paste("Event", i))
  event <- subset(data_str_tr_tt_tags, event == i)
  print(pairwise.wilcox.test(event$CommentCount, event$status, p.adjust.method="none"))
  # print(pairwise.wilcox.test(event$CommentCount, event$status, exact = FALSE))
  print("===============================================")
}


# Start UX
#Base Information
for (i in 1:4) {
  print(subset(data_str_tr_tt_tags, event == i) %>%
          group_by(start_UX) %>%
          tally())
}

# Time count of censured across accepted answers
for (i in 1:4) {
  event <- subset(data_str_tr_tt_tags, event == i) %>%
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
for (i in 1:4) {
  print(subset(data_str_tr_tt_tags, event == i) %>%
          group_by(Autobiographer) %>%
          tally())
}

# Time count of censured across accepted answers
for (i in 1:4) {
  event <- subset(data_str_tr_tt_tags, event == i) %>%
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
for (i in 1:4) {
  print(subset(data_str_tr_tt_tags, event == i) %>%
          group_by(year) %>%
          tally())
}

# Time count of censured across accepted answers
for (i in 1:4) {
  event <- subset(data_str_tr_tt_tags, event == i) %>%
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
for (i in 1:4) {
  print(subset(data_str_tr_tt_tags, event == i) %>%
          summarise(mean = mean(tenure), 
                    median = median(tenure),
                    sdt = sd(tenure)))
}

# Numerical descriptive table 
for (i in 1:4) {
  print(paste("Event", i))
  print(subset(data_str_tr_tt_tags, event == i) %>%
          group_by(status)%>%
          summarise(mean(tenure), median(tenure),sd(tenure)))
  print("===============================================")
}

for (i in 1:4) {
  print(paste("Event", i))
  event <- subset(data_str_tr_tt_tags, event == i)
  print(pairwise.wilcox.test(event$tenure, event$status, p.adjust.method="none"))
  #print(pairwise.wilcox.test(event$EditCount, event$status, exact = FALSE))
  print("===============================================")
}


# weekday
#Base Information
for (i in 1:4) {
  print(subset(data_str_tr_tt_tags, event == i) %>%
          group_by(weekday) %>%
          tally())
}

# Time count of censured across accepted answers
for (i in 1:4) {
  event <- subset(data_str_tr_tt_tags, event == i) %>%
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

