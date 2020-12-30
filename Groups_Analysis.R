# Find out which clusters is better to keep
# considering the average width 

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

## TODO TOO MANY TAGS TRY TO CONSIDER ONLY THE TOP 

TopTags <- TagsTable %>%
  arrange(desc(Count)) %>%
  head(50)

Tags <- merge(Tags, TopTags[, c("TagName", "Count")], by = "TagName", all.x = TRUE)

Tags$TagName <- ifelse(is.na(Tags$Count), "other_tags", Tags$TagName)

vocabulary <- unique(Tags$TagName)

# Construct a document term matrix from a data.frame with columns 
# doc_id (OwnerUserId), term(tag), freq

x <- as.data.frame(table(Tags$OwnerUserId, Tags$TagName))
colnames(x) <- c("doc_id", "term", "freq")

x <- subset(x, freq >0)

require(udpipe)
UserTag <- document_term_matrix(x, vocabulary, weight = "freq")

# SPECTRAL CLUSTERING
#https://stackoverflow.com/questions/29417754/is-there-any-sparse-support-for-dist-function-in-r
# require(wordspace)
#foo <- dist.matrix(UserTag, method="euclidean", as.dist=TRUE)

require(text2vec)
# calculate cosine similarity
UserTagSim <- sim2(UserTag, UserTag, method = "cosine")

# convert cosine similarity to cosine distance by subtracting it from 1
# Subtraction by a Scalar and convert the matrix to a dist object
UserTagDis <- as.dist(1- round(UserTagSim, 6))

# User Clustering
# - hierarchical clustering using Ward's method as merge rule
hc <- hclust(UserTagDis, "ward.D")

require(cluster)

# Use map_dbl to run many models with varying value of k
sil_width <- map_dbl(seq(4000, 5000, by=10),  function(k){
  sil <- silhouette(cutree(hc, k=k), UserTagDis)
  model <- summary(sil)
  model$avg.width
})


# Generate a data frame containing both k and sil_width
sil_df <- data.frame(
  k = seq(4000, 5000, by=10),
  sil_width = sil_width
)

# Plot the relationship between k and sil_width
ggplot(sil_df, aes(x = k, y = sil_width)) +
  geom_line() +
  scale_x_continuous(breaks = seq(4000, 5000, by=10))

# Max silhouette with k = 5654
sil <- silhouette(cutree(hc, k=4570), UserTagDis)
model <- summary(sil)
cluster_size <- as.data.frame(model$clus.sizes)
cluster_size$cl <- as.numeric(as.character(cluster_size$cl))


# store group in the dataset 

sil_df <- data.frame(cluster = as.numeric(), 
                     neighbor = as.numeric(),
                     sil_width = as.numeric(),
                     stringsAsFactors=FALSE) 

# 1. For all the users
for (i in 1: dim(sil)[1]) {
  sil_df[i, ] <- sil[i,]
}

# add OwnerUserId
require(data.table)
setDT(sil_df, keep.rownames = "user_location")

names_reduced_df <- as.data.frame(UserTagSim@Dimnames[[1]])
setDT(names_reduced_df, keep.rownames = "user_location")
colnames(names_reduced_df)[2] <- "OwnerUserId"

sil_df <- merge(sil_df, names_reduced_df, by = "user_location", all.x = TRUE)
sil_df[, 1] <- NULL


sil_df <- merge(sil_df, cluster_size, by.x = "cluster", 
                by.y = "cl", all.x = TRUE)

# Remove observations clustered alone
sil_df <- subset(sil_df, Freq != 1)

# # SILHOUETTE COEFFICIENT INTERPRETATION
# 'Thus an {\displaystyle s(i)}s(i) close to one means that the data is 
# appropriately clustered. If {\displaystyle s(i)}s(i) is close to 
# negative one, then by the same logic we see that {\displaystyle i}i 
# would be more appropriate if it was clustered in its neighbouring cluster.
# An {\displaystyle s(i)}s(i) near zero means that the datum is on the
# border of two natural clusters.'

# Keep only clusters with an average silhouette >= 0.5
# TODO move negative silhouette to neighbor cluster
# TODO merge cluster with silhouette close to zero

cluster_sil_width <- as.data.frame(model$clus.avg.widths)
setDT(cluster_sil_width, keep.rownames = "cluster")
colnames(cluster_sil_width)[2] <- "cluster_avg_widths"
cluster_sil_width$cluster <- as.numeric(cluster_sil_width$cluster)

# why the results change so much?
# Descriptive stats.
sil_df <- merge(sil_df, cluster_sil_width, by = "cluster", all.x = TRUE)
hist(sil_df$cluster_avg_widths)

# cluster tag matrix
tmp <- merge(sil_df, x, by.x = "OwnerUserId", by.y = "doc_id", all.x = TRUE)
tdm <- aggregate(tmp$freq, by=list(cluster=tmp$cluster, tag=tmp$term), FUN=sum)


# A lot of observations with an average cluster of 1
# what are those observations?
cluster_1 <- subset(sil_df, cluster_avg_widths == 1) # 4100 users
length(unique(cluster_1$cluster)) # 574 cluster
summary(cluster_1$Freq) # Mean 15 users per cluster

# Users that answer the same questions and then censured
# for each cluster get max event and count of unique question 
# for the users within the cluster

# Initialize the dataframe and the row count
cluster_1_desc <- data.frame(cluster = as.numeric(), 
                           max_event = as.numeric(),
                           count_question = as.numeric(),
                           stringsAsFactors=FALSE) 
row = 1

# 1. For all the clusters
for (i in unique(cluster_1$cluster)) {
  tmp <- subset(cluster_1, cluster == i) %>%
          select(OwnerUserId)
  tmp <- subset(data_str_tr_tt, OwnerUserId %in% as.numeric(tmp[[1]]))
  
  # Store the result in the dataframe
  cluster_1_desc[row, 1] <- i
  cluster_1_desc[row, 2] <-  max(tmp$event)
  cluster_1_desc[row, 3] <- length(unique(tmp$ParentId))
  
  row = row + 1
}

rm(tmp, i, row)


# COMMENT MOST PEOPLE HAVE ONLY ONE ANSWER TO ONE QUESTION
# SAME TAG USED IN DIFFERENT QUESTIONS

# Higher the silhouette score and more distinct the clusters are

# which tags are in each of these clusters
# Build a term-document matrix: cluster - tag -freq

foo <- merge(cluster_1, x, by.x = "OwnerUserId", by.y = "doc_id", all.x = TRUE)
tdm <- aggregate(foo$freq, by=list(cluster=foo$cluster, tag=foo$term), FUN=sum)


require("wordcloud")
require("RColorBrewer")
bar <- subset(tdm, cluster == 4)
wordcloud(words = bar$tag, freq = bar$x, min.freq = 1,
          max.words=200, random.order=FALSE, rot.per=0.35, 
          colors=brewer.pal(8, "Dark2"))

# What other info lower silhouette cluster adds

cluster_09 <- subset(sil_df, cluster_avg_widths >= 0.9 & 
                       cluster_avg_widths < 1) # +41 users
length(unique(cluster_09$cluster)) # +3 [3348 4472 5468] clusters

foo <- merge(cluster_09, x, by.x = "OwnerUserId", by.y = "doc_id", all.x = TRUE)
tdm <- aggregate(foo$freq, by=list(cluster=foo$cluster, tag=foo$term), FUN=sum)

require("wordcloud")
require("RColorBrewer")
bar <- subset(tdm, cluster == 5468)
wordcloud(words = bar$tag, freq = bar$x, min.freq = 1,
          max.words=200, random.order=FALSE, rot.per=0.35, 
          colors=brewer.pal(8, "Dark2"))

cluster_08 <- subset(sil_df, cluster_avg_widths >= 0.8 & 
                       cluster_avg_widths < 0.9) # +182 users
length(unique(cluster_08$cluster)) # +29 clusters


# for each cluster get max event and count of unique question 
# for the users within the cluster

# Initialize the dataframe and the row count
cluster_08_desc <- data.frame(cluster = as.numeric(), 
                             max_event = as.numeric(),
                             count_question = as.numeric(),
                             stringsAsFactors=FALSE) 
row = 1

# 1. For all the clusters
for (i in unique(cluster_08$cluster)) {
  tmp <- subset(cluster_08, cluster == i) %>%
    select(OwnerUserId)
  tmp <- subset(data_str_tr_tt, OwnerUserId %in% as.numeric(tmp[[1]]))
  
  # Store the result in the dataframe
  cluster_08_desc[row, 1] <- i
  cluster_08_desc[row, 2] <-  max(tmp$event)
  cluster_08_desc[row, 3] <- length(unique(tmp$ParentId))
  
  row = row + 1
}

rm(tmp, i, row)

foo <- merge(cluster_08, x, by.x = "OwnerUserId", by.y = "doc_id", all.x = TRUE)
tdm <- aggregate(foo$freq, by=list(cluster=foo$cluster, tag=foo$term), FUN=sum)

cluster_07 <- subset(sil_df, cluster_avg_widths >= 0.7 & 
                       cluster_avg_widths < 0.8) # +167 users
length(unique(cluster_07$cluster)) # +44 cl

foo <- merge(cluster_07, x, by.x = "OwnerUserId", by.y = "doc_id", all.x = TRUE)
tdm <- aggregate(foo$freq, by=list(cluster=foo$cluster, tag=foo$term), FUN=sum)
