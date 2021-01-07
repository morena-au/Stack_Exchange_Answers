# Descriptive Stats and Plots
library(ggplot2)
require(gridExtra)
library(tidyverse)
library(hrbrthemes)
library(viridis)

# Import file
setwd("C:/Projects/Stack_Exchange/motivation_feedback/Answers/data")
data_str_all_gt <- read.csv(file="./data_str_all_gt.csv", stringsAsFactors=FALSE)

user_freq <- count(data_str_all_gt, OwnerUserId)
colnames(user_freq)[2] <- "Count"

# How many people have at least one recurrence?
user_recurrence_2 <- subset(user_freq, Count > 1)
(nrow(user_recurrence_2)/nrow(user_freq))*100

# Mean number of recurrence
user_freq$recurrence <- user_freq$Count - 1
summary(user_freq$recurrence)

# Count of contributors per total amount of answers given ("strata")
cumulative_answers <- count(user_freq, Count)
colnames(cumulative_answers) <- c("TotAnswers", "Count")

cumulative_answers$sum <- cumsum(cumulative_answers[, 2])
cumulative_answers$pct_total <- round((cumulative_answers$sum/
                                         length(unique(data_str_all_gt$OwnerUserId)))*100, 0)


# Simplify the graph taking into account only the users that answers at most 10 questions
cumulative_answers <- subset(cumulative_answers, TotAnswers <= 10)

p1 <- ggplot(cumulative_answers, aes(x = factor(TotAnswers), y = Count)) +
  geom_bar(stat = "identity", color="black", fill="grey", width = 0.5) +
  geom_text(aes(label=Count), vjust=-1) +
  ggtitle("Censured Contributors") +
  ylab("Contributors Count") + 
  xlab("Total Answers Given") +
  theme(axis.text.y=element_blank(), 
        axis.text.x=element_text(size=10)) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.1)))


p2 <- ggplot(cumulative_answers, aes(x = factor(TotAnswers), y = pct_total)) +
  geom_bar(stat = "identity", color="black", fill="grey", width = 0.5) +
  geom_text(aes(label=paste0(pct_total, "%")), vjust=-1) +
  ggtitle("Percentage of Censured Contributors") +
  ylab("Contributors Percentage") + 
  xlab("Total Answers Given") +
  theme(axis.text.y=element_blank(), 
        axis.text.x=element_text(size=10)) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.1)))

# Output image: answers_seq 1200x400
grid.arrange(p1 + geom_vline(xintercept=4.5, colour = "red"), 
             p2 + geom_vline(xintercept=4.5, colour = "red"), ncol=2)

# Truncate observations after the tenth event
data_str_tr_gt <- subset(data_str_all_gt, event <= 4)

# Save the file
write.csv(data_str_tr_gt, "data_str_tr_gt.csv", row.names = FALSE)

# DESCRIPTIVE STATS
setwd("C:/Projects/Stack_Exchange/motivation_feedback/Answers/data")
data_str_tr_gt <- read.csv("data_str_tr_gt.csv", stringsAsFactors = FALSE)
data_str_tr_gt$event <- factor(data_str_tr_gt$event)
data_str_tr_gt$status <- factor(data_str_tr_gt$status)
data_str_tr_gt$AcceptedByOriginator <- factor(data_str_tr_gt$AcceptedByOriginator)
data_str_tr_gt$TimeBetweenAnswer <- data_str_tr_gt$tstop - data_str_tr_gt$tstart

# Transform in days
data_str_tr_gt$TimeBetweenAnswer <- round(data_str_tr_gt$TimeBetweenAnswer/(24*60*60), 0)

# Base Information
table(data_str_tr_gt$status, data_str_tr_gt$event)

## COVARIATES
# - "AcceptedByOriginator" 
summary(data_str_tr_gt$AcceptedByOriginator)

#Base Information
for (i in unique(data_str_tr_gt$event)) {
  print(subset(data_str_tr_gt, event == i) %>%
          group_by(AcceptedByOriginator) %>%
          tally())
}

# Time between answers in days
for (i in unique(data_str_tr_gt$event)) {
  print(subset(data_str_tr_gt, event == i) %>%
    group_by(AcceptedByOriginator)%>%
    summarise(median(TimeBetweenAnswer),sd(TimeBetweenAnswer)))
}

# Compute paired-sample Wilcoxon test

for (i in unique(data_str_tr_gt$event)) {
  event <- subset(data_str_tr_gt, event == i)
  print(paste("Event", i))
  print(pairwise.wilcox.test(event$TimeBetweenAnswer, event$AcceptedByOriginator, p.adjust.method="none"))
  print(pairwise.wilcox.test(event$TimeBetweenAnswer, event$AcceptedByOriginator, exact = FALSE))
}

# Time count of censured across accepted answers
for (i in unique(data_str_tr_gt$event)) {
  event <- subset(data_str_tr_gt, event == i) %>%
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

for (i in unique(data_str_tr_gt$event)) {
  print(subset(data_str_tr_gt, event == i) %>%
          tally(EditCount))
}

# Numerical descriptive table 
for (i in unique(data_str_tr_gt$event)) {
  print(paste("Event", i))
  print(subset(data_str_tr_gt, event == i) %>%
          group_by(status)%>%
          summarise(round(mean(EditCount),3), median(EditCount),sd(EditCount)))
  print("===============================================")
}

for (i in unique(data_str_tr_gt$event)) {
  print(paste("Event", i))
  event <- subset(data_str_tr_gt, event == i)
  print(pairwise.wilcox.test(event$EditCount, event$status, p.adjust.method="none"))
  #print(pairwise.wilcox.test(event$EditCount, event$status, exact = FALSE))
  print("===============================================")
}

# - "UpMod"  

for (i in unique(data_str_tr_gt$event)) {
  print(subset(data_str_tr_gt, event == i) %>%
          tally(UpMod))
}


for (i in unique(data_str_tr_gt$event)) {
  print(paste("Event", i))
  print(subset(data_str_tr_gt, event == i) %>%
          group_by(status)%>%
          summarise(round(mean(UpMod), 3), median(UpMod),sd(UpMod)))
  print("===============================================")
}

for (i in unique(data_str_tr_gt$event)) {
  print(paste("Event", i))
  event <- subset(data_str_tr_gt, event == i)
  print(pairwise.wilcox.test(event$UpMod, event$status, p.adjust.method="none"))
  # print(pairwise.wilcox.test(event$UpMod, event$status, exact = FALSE))
  print("===============================================")
}


# - "DownMod" 

for (i in unique(data_str_tr_gt$event)) {
  print(subset(data_str_tr_gt, event == i) %>%
          tally(DownMod))
}


for (i in unique(data_str_tr_gt$event)) {
  print(paste("Event", i))
  print(subset(data_str_tr_gt, event == i) %>%
          group_by(status)%>%
          summarise(round(mean(DownMod), 3), median(DownMod),sd(DownMod)))
  print("===============================================")
}

for (i in unique(data_str_tr_gt$event)) {
  print(paste("Event", i))
  event <- subset(data_str_tr_gt, event == i)
  print(pairwise.wilcox.test(event$DownMod, event$status, p.adjust.method="none"))
  # print(pairwise.wilcox.test(event$DownMod, event$status, exact = FALSE))
  print("===============================================")
}

# - "CommentCount"

for (i in unique(data_str_tr_gt$event)) {
  print(subset(data_str_tr_gt, event == i) %>%
          tally(CommentCount))
}


for (i in unique(data_str_tr_gt$event)) {
  print(paste("Event", i))
  print(subset(data_str_tr_gt, event == i) %>%
          group_by(status)%>%
          summarise(round(mean(CommentCount), 3), median(CommentCount),sd(CommentCount)))
  print("===============================================")
}

for (i in unique(data_str_tr_gt$event)) {
  print(paste("Event", i))
  event <- subset(data_str_tr_gt, event == i)
  print(pairwise.wilcox.test(event$CommentCount, event$status, p.adjust.method="none"))
  # print(pairwise.wilcox.test(event$CommentCount, event$status, exact = FALSE))
  print("===============================================")
}


