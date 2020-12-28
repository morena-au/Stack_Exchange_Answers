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

plot(hc, main = "Hierarchical clustering of 8502 OwnerUserId based on Tags they answered on",
     ylab = "", xlab = "", yaxt = "n")

rect.hclust(hc, 50, border = "red")

require(cluster)

# Max silhouettte with k = 5660
sil <- silhouette(cutree(hc, k=5660), UserTagDis)
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


sil_df <- merge(sil_df, cluster_size, by.x = "cluster", by.y = "cl", all.x = TRUE)

# Remove observations clustered alone
sil_df <- subset(sil_df, Freq != 1)

# SILHOUETTE COEFFICIENT INTERPRETATION
'Thus an {\displaystyle s(i)}s(i) close to one means that the data is 
appropriately clustered. If {\displaystyle s(i)}s(i) is close to 
negative one, then by the same logic we see that {\displaystyle i}i 
would be more appropriate if it was clustered in its neighbouring cluster.
An {\displaystyle s(i)}s(i) near zero means that the datum is on the
border of two natural clusters.'

# Keep only clusters with an average silhouette >= 0.5
# TODO move negative silhouette to neighbor cluster
# TODO merge cluster with silhouette close to zero

cluster_sil_width <- as.data.frame(model$clus.avg.widths)
setDT(cluster_sil_width, keep.rownames = "cluster")
colnames(cluster_sil_width)[2] <- "avg_widths"

cluster_sil_width <- subset(cluster_sil_width, avg_widths >= 0.4) # Tuning params
sil_df_00 <- subset(sil_df, cluster %in% cluster_sil_width$cluster)

# Move the negative silhouette to the neighbor cluster
sil_df_00$cluster <- ifelse(sil_df_00$sil_width < 0, 
                            sil_df_00$neighbor, 
                            sil_df_00$cluster)

data_str_tr_tt <- subset(data_str_tr_tt, OwnerUserId %in% sil_df_00$OwnerUserId)

data_str_tr_tt <- merge(data_str_tr_tt, sil_df_00[, c("cluster", "OwnerUserId")], 
      by = "OwnerUserId", all.x = TRUE)

data_str_tr_tt <- subset(data_str_tr_tt, select = -c(TagName, TagFreq))
colnames(data_str_tr_tt)[dim(data_str_tr_tt)[2]] <- "TagCluster"

# Save the file
setwd("C:/Projects/Stack_Exchange/motivation_feedback/Answers/data")
write.csv(data_str_tr_tt, "data_str_tr_tt_00_04.csv", row.names = FALSE)

# Gap Time 
data_str_tr_gt <- read.csv("data_str_tr_gt.csv", stringsAsFactors = FALSE)
data_str_tr_gt <- subset(data_str_tr_gt, OwnerUserId %in% sil_df_00$OwnerUserId)
data_str_tr_gt <- merge(data_str_tr_gt, sil_df_00[, c("cluster", "OwnerUserId")], 
                        by = "OwnerUserId", all.x = TRUE)

colnames(data_str_tr_gt)[dim(data_str_tr_gt)[2]] <- "TagCluster"
write.csv(data_str_tr_gt, "data_str_tr_gt_00_04.csv", row.names = FALSE)

# TODO contributors who answered more question are penalized 
