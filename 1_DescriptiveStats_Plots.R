# Descriptive Stats and Plots
library(ggplot2)
require(gridExtra)

# Import file
setwd("C:/Projects/Stack_Exchange/motivation_feedback/Answers/data")
data_str_all <- read.csv(file="./data_str_all.csv", stringsAsFactors=FALSE)

# Truncation is necessary also for PWP-TT (braga2018recurrent) 

user_freq <- count(data_str_all, OwnerUserId)
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
                                         length(unique(data_str_all$OwnerUserId)))*100, 0)


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
  scale_y_continuous(expand = expansion(mult = c(0, 0.1))) +
  

grid.arrange(p1 + geom_vline(xintercept=4.5, colour = "red"), 
             p2 + geom_vline(xintercept=4.5, colour = "red"), ncol=2)

# Truncate observations after the tenth event
data_str_tr <- subset(data_str_all, event <= 4)

# Save the file
write.csv(data_str_tr, "data_str_tr.csv", row.names = FALSE)

## Descriptive statistics




hist(log(data_str_all$TimeBetweenAnswers))