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

# ## TODO TOO MANY TAGS TRY TO CONSIDER ONLY THE TOP 
# 
# TopTags <- TagsTable %>%
#   arrange(desc(Count)) %>%
#   head(100)

Tags <- merge(Tags, TagsTable[, c("TagName", "Count")], by = "TagName", all.x = TRUE)

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

# # Use map_dbl to run many models with varying value of k
# sil_width <- map_dbl(seq(5000, 6500, by=10),  function(k){
#   sil <- silhouette(cutree(hc, k=k), UserTagDis)
#   model <- summary(sil)
#   model$avg.width
# })
# 
# 
# # Generate a data frame containing both k and sil_width
# sil_df <- data.frame(
#   k = seq(5000, 6500, by=10),
#   sil_width = sil_width
# )
# 
# # Plot the relationship between k and sil_width
# ggplot(sil_df, aes(x = k, y = sil_width)) +
#   geom_line() +
#   scale_x_continuous(breaks = seq(5000, 6500, by=10))

# Silhouette with k= 5650 with all the tags - sil avg 0.3192589
# Max silhouette with k = 5100 with top tags 100 - sil avg 0.4098277
# Max silhouette with k = 5300 with top tags 200 - sil avg 0.34
# Max silhouette with k = 4570 with top tags 50 - sil avg 0.53
sil <- silhouette(cutree(hc, k=5650), UserTagDis)
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

setwd("C:/Projects/Stack_Exchange/motivation_feedback/Answers/data")
write.csv(tdm, "tdm_all.csv", row.names = FALSE)

# Move the negative silhouette to the neighbor cluster
sil_df$cluster <- ifelse(sil_df$sil_width < 0, 
                            sil_df$neighbor, 
                            sil_df$cluster)

setwd("C:/Projects/Stack_Exchange/motivation_feedback/Answers/data")
write.csv(sil_df, "silhouette_df_all.csv", row.names = FALSE)

data_str_tr_tt <- subset(data_str_tr_tt, OwnerUserId %in% sil_df$OwnerUserId)

data_str_tr_tt <- merge(data_str_tr_tt, sil_df[, c("cluster", "OwnerUserId")], 
                        by = "OwnerUserId", all.x = TRUE)

data_str_tr_tt <- subset(data_str_tr_tt, select = -c(TagName, TagFreq))
colnames(data_str_tr_tt)[dim(data_str_tr_tt)[2]] <- "TagCluster"


data_str_tr_tt$weekday <- ifelse(data_str_tr_tt$day %in% c("Monday", "Tuesday", 
                                                           "Wednesday", "Thursday", 
                                                           "Friday"), 1, 0)
data_str_tr_tt$day <- NULL

# Make sure that the tag cluster have at least one different
# treatment manifestation within the cluster

TagCluster_tt <- data_str_tr_tt %>%
  group_by(TagCluster) %>%
  summarise(UniqueEdit = length(unique(EditCount)),
            UniqueAcc = length(unique(AcceptedByOriginator)),
            UniqueUp = length(unique(UpMod)),
            UniqueMod = length(unique(DownMod)),
            UniqueComm = length(unique(CommentCount))) %>%
  select(TagCluster, UniqueEdit, UniqueAcc, 
         UniqueUp, UniqueMod, UniqueComm)

TagCluster_tt <- subset(TagCluster_tt, !(UniqueEdit == 1 & UniqueAcc == 1 & UniqueUp == 1 &
                                           UniqueMod == 1 & UniqueComm == 1))

# Subset gt by the clusters who have at least one different treatment
data_str_tr_tt <- subset(data_str_tr_tt, TagCluster %in% TagCluster_tt$TagCluster)

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

# Save the file
setwd("C:/Projects/Stack_Exchange/motivation_feedback/Answers/data")
write.csv(data_str_tr_tt, "data_str_tr_tt_all.csv", row.names = FALSE)

# Gap Time 
data_str_tr_gt <- read.csv("data_str_tr_gt.csv", stringsAsFactors = FALSE)
data_str_tr_gt <- subset(data_str_tr_gt, OwnerUserId %in% sil_df$OwnerUserId)
data_str_tr_gt <- merge(data_str_tr_gt, sil_df[, c("cluster", "OwnerUserId")], 
                        by = "OwnerUserId", all.x = TRUE)

colnames(data_str_tr_gt)[dim(data_str_tr_gt)[2]] <- "TagCluster"

# remove QuestionTag and add AutobiographerDate
data_str_tr_gt <- subset(data_str_tr_gt, select = -c(QuestionTag))

# Add badges information
setwd("C:/Projects/Stack_Exchange/motivation_feedback/Answers/data")
Badges <- read.csv(file="./raw/Badges.csv",stringsAsFactors=FALSE)
keep <- c("UserId", "Name", "Date") 

Badges <- Badges[keep]
Badges <- subset(Badges, Name == "Autobiographer")

colnames(Badges)[3] <- "AutobiographerDate"

data_str_tr_gt <- merge(data_str_tr_gt, Badges[, c("UserId", "AutobiographerDate")], 
                      by.x = "OwnerUserId", by.y = "UserId", all.x = TRUE)


# Create a weekday and a weekend variable
data_str_tr_gt$weekday <- ifelse(data_str_tr_gt$day %in% c("Monday", "Tuesday", 
                                                           "Wednesday", "Thursday", 
                                                           "Friday"), 1, 0)

data_str_tr_gt$day <- NULL

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

data_str_tr_gt$AutobiographerDate <- NULL

# Save the file
setwd("C:/Projects/Stack_Exchange/motivation_feedback/Answers/data")
write.csv(data_str_tr_gt, "data_str_tr_gt_all.csv", row.names = FALSE)





