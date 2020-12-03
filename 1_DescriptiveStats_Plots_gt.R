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
(4991/11778)*100 #42%

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
  ggtitle("Count of contributors per total amount of answers given") +
  ylab("Contributors Count") + 
  xlab("Total Answers Given") +
  theme(axis.text.y=element_blank(), 
        axis.text.x=element_text(size=10)) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.1)))


p2 <- ggplot(cumulative_answers, aes(x = factor(TotAnswers), y = pct_total)) +
  geom_bar(stat = "identity", color="black", fill="grey", width = 0.5) +
  geom_text(aes(label=paste0(pct_total, "%")), vjust=-1) +
  ggtitle("Percentage of contributors per total amount of answers given") +
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
data_str_tr_gt <- read.csv("data_str_tr_gt.csv", stringsAsFactors = FALSE)

# DESCRIPTIVE STATS
## COVARIATES
# - "AcceptedByOriginator" 
data_str_tr_gt$event <- factor(data_str_tr_gt$event)
data_str_tr_gt$status <- factor(data_str_tr_gt$status)
data_str_tr_gt$AcceptedByOriginator <- factor(data_str_tr_gt$AcceptedByOriginator)
data_str_tr_gt$TimeBetweenAnswer <- data_str_tr_gt$tstop - data_str_tr_gt$tstart
# Transform in days
data_str_tr_gt$TimeBetweenAnswer <- round(data_str_tr_gt$TimeBetweenAnswer/(24*60*60), 3)

table(data_str_tr_gt$AcceptedByOriginator, data_str_tr_gt$status)

data_str_tr_gt %>%
  ggplot( aes(x=AcceptedByOriginator, y=TimeBetweenAnswer, fill=AcceptedByOriginator)) +
  #geom_boxplot() +
  scale_fill_viridis(discrete = TRUE, alpha=0.6, option="A") +
  geom_jitter(color="black", size=0.4, alpha=0.9) +
  theme(
    legend.position="none",
    plot.title = element_text(size=11)
  ) +
  ggtitle("Violin chart") +
  xlab("")

ggplot(data_str_tr_gt, 
       aes(x = AcceptedByOriginator, 
           fill = status)) + 
  geom_bar(position = "stack")

# - "EditCount" 
# - "UpMod"                
# - "DownMod"  
data_str_tr_gt %>%
  ggplot( aes(x=event, y=DownMod, fill=event)) +
  #geom_boxplot() +
  scale_fill_viridis(discrete = TRUE, alpha=0.6, option="A") +
  geom_jitter(color="black", size=0.4, alpha=0.9) +
  theme(
    legend.position="none",
    plot.title = element_text(size=11)
  ) +
  ggtitle("Violin chart") +
  xlab("")

data_str_tr_gt %>%
  ggplot( aes(x=status, y=DownMod, fill=status)) +
  geom_violin() +
  scale_fill_viridis(discrete = TRUE, alpha=0.6, option="A") +
  theme(
    legend.position="none",
    plot.title = element_text(size=11)
  ) +
  ggtitle("Violin chart") +
  xlab("")

# - "CommentCount"
data_str_tr_gt %>%
  ggplot( aes(x=event, y=CommentCount, fill=event)) +
  #geom_boxplot() +
  scale_fill_viridis(discrete = TRUE, alpha=0.6, option="A") +
  geom_jitter(color="black", size=0.4, alpha=0.9) +
  theme(
    legend.position="none",
    plot.title = element_text(size=11)
  ) +
  ggtitle("Violin chart") +
  xlab("")

data_str_tr_gt %>%
  ggplot( aes(x=status, y=CommentCount, fill=status)) +
  geom_violin() +
  scale_fill_viridis(discrete = TRUE, alpha=0.6, option="A") +
  theme(
    legend.position="none",
    plot.title = element_text(size=11)
  ) +
  ggtitle("Violin chart") +
  xlab("")

table(data_str_tr_gt$CommentCount, data_str_tr_gt$status)
