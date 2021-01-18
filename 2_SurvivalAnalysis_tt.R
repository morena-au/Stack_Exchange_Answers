library(dplyr)
library(survival)
library(ggplot2)
library(viridis)

# Import file
setwd("C:/Projects/Stack_Exchange/motivation_feedback/Answers/data")
data_str_tr_tt_tags <- read.csv("data_str_tr_tt_all.csv", stringsAsFactors = FALSE)
data_str_tr_tt <- read.csv("data_str_tr_tt.csv", stringsAsFactors = FALSE)
tdm <- read.csv("tdm_all.csv", stringsAsFactors = FALSE)
silhouette_df <- read.csv("silhouette_df_all.csv", stringsAsFactors = FALSE)


cluster_df <- as.data.frame(table(data_str_tr_tt_tags$TagCluster))

# # adjust for data_str_tt variables
# data_str_tr_tt$weekday <- ifelse(data_str_tr_tt$day %in% c("Monday", "Tuesday", 
#                                                            "Wednesday", "Thursday", 
#                                                            "Friday"), 1, 0)
# data_str_tr_tt$day <- NULL
# 
# data_str_tr_tt <- data_str_tr_tt[ , -which(names(data_str_tr_tt) %in% c("TagName","TagFreq"))]
# 
# # Did the user received the autobiographer badge before answering the first question
# # shows how committed users are within the platform (use as advertisement tools)
# 
# first_event <- data_str_tr_tt %>%
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
# data_str_tr_tt <- merge(data_str_tr_tt, first_event[, c("OwnerUserId", "Autobiographer")], 
#                         by = "OwnerUserId", all.x = TRUE)
# 
# data_str_tr_tt$AutobiographerDate <- NULL
# 
# # Save the file
# write.csv(data_str_tr_tt, "data_str_tr_tt.csv", row.names = FALSE)

# add reputation 
setwd("C:/Projects/Stack_Exchange/motivation_feedback/Answers/data/raw")
reputation <- read.csv("Reputation.csv", stringsAsFactors = FALSE) 

# time formatting
reputation$time_UTC <- as.POSIXct(substr(reputation$time_UTC,1,nchar(reputation$time_UTC)-1), 
                                  format = "%Y-%m-%d %H:%M:%S", tz = "UTC")

# Calculate totRep until CreationTime
reputation_df <- merge(data_str_tr_tt[,c("OwnerUserId", "Id", "CreationDate")], reputation, 
                       by = "OwnerUserId", all.x = TRUE)


reputation_sum <- reputation_df %>%
  group_by(Id) %>%
  summarize(TotRep = sum(reputation[time_UTC <= CreationDate]))

# add 1 reputation points to all. 
# 1 point is given as default when you register 
reputation_sum$TotRep <- ifelse(is.na(reputation_sum$TotRep), 0, reputation_sum$TotRep)
reputation_sum$TotRep <- reputation_sum$TotRep + 1

# Merge
data_str_tr_tt <- merge(data_str_tr_tt, reputation_sum, 
                        by = "Id", all.x = TRUE)


# Adjust the variables
data_str_tr_tt$year <- factor(data_str_tr_tt$year) # year the answer was asked
data_str_tr_tt$start_UX <- factor(data_str_tr_tt$start_UX) # the contributor joined UX first
# data_str_tr_tt$tenure <- factor(data_str_tr_tt$tenure) # since when the contributor registered to one community in SE
data_str_tr_tt$weekday <- factor(data_str_tr_tt$weekday) # contributor posted during weekdays or weekends
data_str_tr_tt$Autobiographer <- factor(data_str_tr_tt$Autobiographer)
data_str_tr_tt$EditCount <- factor(data_str_tr_tt$EditCount)
data_str_tr_tt$EditCount <- as.numeric(as.character(data_str_tr_tt$EditCount))
# EditDummy
data_str_tr_tt$EditDummy <- ifelse(data_str_tr_tt$EditCount > 0, 1, 0)
data_str_tr_tt$EditDummy <- factor(data_str_tr_tt$EditDummy)
# CommentDummy
data_str_tr_tt$CommentDummy <- ifelse(data_str_tr_tt$CommentCount > 0, 1, 0)
data_str_tr_tt$CommentDummy <- factor(data_str_tr_tt$CommentDummy)

# CommentDummy
data_str_tr_tt$AcceptedDummy <- ifelse(data_str_tr_tt$AcceptedByOriginator > 0, 1, 0)
data_str_tr_tt$AcceptedDummy <- factor(data_str_tr_tt$AcceptedDummy)

# standardize tenure
data_str_tr_tt$tenure_std <- scale(data_str_tr_tt$tenure)

data_str_tr_tt$score <- data_str_tr_tt$UpMod - data_str_tr_tt$DownMod

# Create tenure within the UX and tenure with SE (year when first join SE independently 
# from the community)

data_str_tr_tt$tenure_UX <- substring(data_str_tr_tt$UX_registration, 1, 4)
data_str_tr_tt$tenure_UX <- as.numeric(data_str_tr_tt$tenure_UX)

data_str_tr_tt$tenure_SE <- substring(data_str_tr_tt$SE_registration, 1, 4)
data_str_tr_tt$tenure_SE <- as.numeric(data_str_tr_tt$tenure_SE)

# tt Model with clusters
model_pwp_tt_00 = coxph(Surv(tstart, tstop, status) ~
                          UpMod +
                          DownMod +
                          AcceptedByOriginator +
                          EditCount +
                          CommentCount +
                          # start_UX +
                          Autobiographer +
                          year +
                          tenure_UX +
                          tenure_SE +
                          weekday +
                          TotRep +
                          cluster(OwnerUserId) + strata(event), method="breslow", data=data_str_tr_tt, robust = TRUE)

summary(model_pwp_tt_00)


data_str_tr_tt %>%
  ggplot( aes(x=event, y=CommentCount, fill=event)) +
  #geom_boxplot() +
  geom_jitter(color="black", size=0.4, alpha=0.9) +
  theme(
    legend.position="none",
    plot.title = element_text(size=11)
  ) +
  ggtitle("Violin chart") +
  xlab("")

data_str_tr_tt %>%
  ggplot( aes(x=status, y=CommentCount, fill=status)) +
  #geom_boxplot() +
  geom_jitter(color="black", size=0.4, alpha=0.9) +
  theme(
    legend.position="none",
    plot.title = element_text(size=11)
  ) +
  ggtitle("Violin chart") +
  xlab("")

table(data_str_tr_tt$status, data_str_tr_tt$Autobiographer)
table(data_str_tr_tt$start_UX, data_str_tr_tt$Autobiographer)

foo <- subset(data_str_tr_tt, status == 0)
table(foo$start_UX, foo$Autobiographer)

foo <- subset(data_str_tr_tt, status == 1)
table(foo$start_UX, foo$Autobiographer)

# #Base Information
# for (i in 1:4) {
#   print(subset(data_str_tr_tt, event == i) %>%
#           group_by(EditDummy) %>%
#           tally())
# }
# 
# # Time count of censured across accepted answers
# for (i in 1:4) {
#   event <- subset(data_str_tr_tt, event == i) %>%
#     group_by(EditDummy, status) %>%
#     tally()
#   
#   # Calculate the totals by column
#   total <- aggregate(event$n, by=list(Status = event$status), FUN = sum)
#   total <- matrix(c(total$x), nrow = 1, ncol = 2, byrow = TRUE)
#   
#   if (length(event$n) == 2) {
#     event <- matrix(c(event$n, 0, 0), nrow = 2, ncol = 2, byrow = TRUE)
#   } else {
#     event <- matrix(c(event$n), nrow = 2, ncol = 2, byrow = TRUE)
#   }
#   
#   print(paste("EVENT ", i))
#   print(event)
#   
#   event_prop <- matrix(c(0, 0, 0, 0), nrow = 2, ncol = 2, byrow = TRUE)
#   
#   event_prop[,1] <- round((event[,1]/total[,1])*100, 2)
#   event_prop[,2] <- round((event[,2]/total[,2])*100, 2)
#   
#   print("Observed Proportions")
#   print(event_prop)
#   
#   
#   print("Chi-squared Test")
#   print(chisq.test(event))
#   
#   print("#################################")
# }
# 


# Calculate days until next event
# Transform in days
data_str_tr_tt$TimeBetweenAnswer <- data_str_tr_tt$tstop - data_str_tr_tt$tstart
data_str_tr_tt$TimeBetweenAnswer <- round(data_str_tr_tt$TimeBetweenAnswer/(24*60*60), 0)

# Time between answers in days
for (i in unique(data_str_tr_tt$event)) {
  print(subset(data_str_tr_tt, event == i) %>%
          group_by(EditDummy)%>%
          summarise(median(TimeBetweenAnswer),sd(TimeBetweenAnswer)))
}

# Compute paired-sample Wilcoxon test

for (i in unique(data_str_tr_tt$event)) {
  event <- subset(data_str_tr_tt, event == i)
  print(paste("Event", i))
  print(pairwise.wilcox.test(event$TimeBetweenAnswer, event$EditDummy, p.adjust.method="none"))
  print(pairwise.wilcox.test(event$TimeBetweenAnswer, event$EditDummy, exact = FALSE))
}

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

