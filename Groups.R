# Descriptive Stats and Plots
library(tidyverse)

# Import file
setwd("C:/Projects/Stack_Exchange/motivation_feedback/Answers/data")
data_str_tr_tt <- read.csv("data_str_tr_tt.csv", stringsAsFactors = FALSE)

# Import Tags
setwd("C:/Projects/Stack_Exchange/motivation_feedback/Answers/data/raw")
Tags <- read.csv(file="Tags.csv",stringsAsFactors=FALSE)
TagsTable <- read.csv(file="TagsTable.csv",stringsAsFactors=FALSE)

Tags <- merge(Tags, TagsTable[, c("Id", "TagName", "Count")], 
              by.x = "TagId", by.y = "Id", all.x = TRUE)

# Group users based on the tags they answer questions on
Tags$Count <- NULL

# Doing this only tags for the first four questions 
# answered will be taken into consideration
Tags <- merge(data_str_tr_tt[, c("ParentId", "OwnerUserId")], Tags[,c("PostId", "TagName")], 
              by.x = "ParentId", by.y = "PostId", all.x = TRUE)

Tags$TagName <- ifelse(is.na(Tags$TagName), "no-tags", Tags$TagName)

vocabulary <- unique(Tags$TagName)

# Construct a document term matrix from a data.frame with columns 
# doc_id (OwnerUserId), term(tag), freq

x <- as.data.frame(table(Tags$OwnerUserId, Tags$TagName))
colnames(x) <- c("doc_id", "term", "freq")

x <- subset(x, freq >0)

require(udpipe)
UserTag <- document_term_matrix(x, vocabulary, weight = "freq")
#https://stackoverflow.com/questions/29417754/is-there-any-sparse-support-for-dist-function-in-r
# require(wordspace)
#foo <- dist.matrix(UserTag, method="euclidean", as.dist=TRUE)

require(text2vec)
# calculate cosine similarity
UserTagSim <- sim2(UserTag, UserTag, method = "cosine")

# List of users to be removed (with unique tags patterns)
# names in the matrix
names <- as.list(UserTagSim@Dimnames[[1]])

# Collect users with low cosine > 0.50 similarity
# initiate an empty list
user_name = list()
user_position = list()

for (i in c(1:dim(UserTagSim)[1])) {
  tmp <- UserTagSim[i,]
  tmp <- as.list(tmp)
  # cosine similarity of Users with itself is always equal to one thus remove it
  tmp[[i]] <- NULL 
  if (max(unlist(tmp)) <= 0.7) {
    if(length(user_name) == 0) {
      user_name <- as.numeric(names[[i]])
    } else {
      user_name[[length(user_name) + 1]] <- as.numeric(names[[i]])
    }
    if(length(user_position) == 0) {
      user_position <- i
    } else {
      user_position[[length(user_position) + 1]] <- i
    }
  }
}

# # Users that answer more questions are more penalized 
# # since they followed diverse tags path
# match(97981, user_name)
# user_position[311]
# bar <- as.data.frame(UserTag[3897,])
# foo <- subset(data_str_tr_tt, OwnerUserId == 97981)

# Remove rows and columns in the UserTagSim
# TODO if we group for the first tag or most frequent tag per question
# we create more homogeneous groups
# TODO considering all tags we group users who asked the same questions

UserTagSimReduced <- UserTagSim[-do.call(c, as.list(user_position)),
                                -do.call(c, as.list(user_position))]

# convert cosine similarity to cosine distance by subtracting it from 1
# ref: https://cran.r-project.org/web/packages/textmineR/vignettes/b_document_clustering.html
UserTagSimReduced@x <- round(UserTagSimReduced@x, 6) - rep(1,UserTagSimReduced@Dim[1])[UserTagSimReduced@i+1]

# convert the matrix to a dist object
UserTagDis <- as.dist(UserTagSimReduced)

# User Clustering
# - hierarchical clustering using Ward's method as merge rule
hc <- hclust(UserTagDis, "ward.D")

plot(hc, main = "Hierarchical clustering of 8502 OwnerUserId based on Tags they answered on",
     ylab = "", xlab = "", yaxt = "n")

rect.hclust(hc, 50, border = "red")

require(cluster)

# https://www.datanovia.com/en/lessons/determining-the-optimal-number-of-clusters-3-must-know-methods/
# https://www.rdocumentation.org/packages/cluster/versions/2.1.0/topics/silhouette

# Use map_dbl to run many models with varying value of k
sil_width <- map_dbl(2:500,  function(k){
  sil <- silhouette(cutree(hc, k=k), UserTagDis)
  model <- summary(sil)
  model$avg.width
})

# Generate a data frame containing both k and sil_width
sil_df <- data.frame(
  k = 2:500,
  sil_width = sil_width
)

# Plot the relationship between k and sil_width
ggplot(sil_df, aes(x = k, y = sil_width)) +
  geom_line() +
  scale_x_continuous(breaks = 2:500)



## silhouette average is very bed
# TODOfor each group delete all the observation that makes the coeff negative and run it agin 
# https://www.datanovia.com/en/lessons/cluster-validation-statistics-must-know-methods/
# https://stackoverflow.com/questions/30261435/r-clustering-silhouette-with-observation-labels



str(si <- silhouette(cutree(hc, k=200), UserTagDis))
summary(si)




# Control for attitude and intentions > which community joined first
setwd("C:/Projects/Stack_Exchange/motivation_feedback/Answers/data/raw")
AssociatedInfo <- read.csv("users_associated_info.csv", stringsAsFactors = FALSE)

# Remove duplicate
AssociatedInfo <- data.frame(AssociatedInfo[!duplicated(AssociatedInfo), ])

AssociatedInfo$creation_date <- as.POSIXct(AssociatedInfo$creation_date,
                                           origin="1970-01-01",
                                           tz='UTC')