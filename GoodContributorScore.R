library(plyr)
library(dplyr)
library(tidyr)
library(stringr)
library(hunspell)
library(textutils)
library(XML)

# Import file

setwd("C:/Projects/Stack_Exchange/01_motivation_feedback/Answers/data")
data_str_tr <- read.csv("data_str_tr_04_gt_ft.csv", stringsAsFactors = FALSE)


# Deduce good contributors by the first answer's characteristics
data_str_tr <- subset(data_str_tr, event == 1)

# drop columns
data_str_tr <- data_str_tr[ , -which(names(data_str_tr) %in%
                                       c("CreationDate", "LastActivityDate", "LastAccessDate", 
                                         "tstart", "tstop", "event", "status", "UX_registration", 
                                         "EditCount", "AcceptedByOriginator",  "UpMod", "DownMod", 
                                         "CommentCount", "year", "SE_registration", "start_UX",  
                                         "AccountId", "start_tenure", "tenure",
                                         "weekday", "Autobiographer"))]


# 1. Changes the author made on their own answers 
# Consider all the changes the answer went through before the next post
setwd("C:/Projects/Stack_Exchange/01_motivation_feedback/Answers/data/raw")
PostHistory <- read.csv(file="./PostHistory.csv",stringsAsFactors=FALSE)
PostHistoryTypes <- read.csv("./PostHistoryTypes.csv", stringsAsFactors = FALSE)

keep <- c("PostHistoryTypeId", "PostId", "CreationDate", "UserId")
PostHistory <- PostHistory[keep]
PostHistory <- merge(PostHistory, PostHistoryTypes, 
                     by.x = "PostHistoryTypeId", by.y = "Id", all.x = TRUE)
rm(PostHistoryTypes)

# subset to changes in the answer made by the same author of the post 
PostHistory <- subset(PostHistory, PostId %in% unique(data_str_tr$Id))
PostHistory <- merge(PostHistory, data_str_tr[, c("Id", "OwnerUserId")], 
                     by.x = "PostId", by.y = "Id", all.x = TRUE)

PostHistory <- subset(PostHistory, UserId == OwnerUserId)

# keep only EditBody
PostHistory <- subset(PostHistory, PostHistoryTypeId == 5)

# Changed their first answer
PostHistory <- PostHistory[, which(names(PostHistory) %in% c("PostId", "Name"))]
PostHistory <- PostHistory[!duplicated(PostHistory), ]

data_str_tr <- merge(data_str_tr, PostHistory, 
                     by.x = "Id", by.y = "PostId", all.x = TRUE)

names(data_str_tr)[length(names(data_str_tr))] <- "EditAnswer"
data_str_tr$EditAnswer <- ifelse(is.na(data_str_tr$EditAnswer), 0, 1)
rm(PostHistory)

# 2 Answer given on their own question
# Import question
setwd("C:/Projects/Stack_Exchange/01_motivation_feedback/Answers/data/raw")
d.ux.q.00 <- read.csv(file="./d.ux.q.00.csv", stringsAsFactors=FALSE)

keep <- c("Id", "OwnerUserId")
d.ux.q.00 <- d.ux.q.00[, which(names(d.ux.q.00) %in% keep)]
names(d.ux.q.00)[length(names(d.ux.q.00))] <- "q_OwnerUserId"

data_str_tr <- merge(data_str_tr, d.ux.q.00, 
                     by.x = "ParentId", by.y = "Id", all.x = TRUE)


data_str_tr$AnswerOwnQuestion <- ifelse(data_str_tr$OwnerUserId == data_str_tr$q_OwnerUserId, 1, 0)

# NA from question given by deleted users/ unregistered
data_str_tr$AnswerOwnQuestion <- ifelse(is.na(data_str_tr$q_OwnerUserId), 0, data_str_tr$AnswerOwnQuestion)
data_str_tr$q_OwnerUserId <- NULL
rm(d.ux.q.00)

# 3 Link to external resources + context on Initial Body

# ## Revisions API call
# # Create urls for call from scrapy to the revision id api
# 
# revisions <- data.frame(paste0("https://api.stackexchange.com/2.2/posts/", 
#                                unique(data_str_tr$Id),
#                                "/revisions?site=ux&filter=withbody&page=1&pagesize=100&key=G0yd6IHl5kBtkBtsNU*4dg(("))
# names(revisions) <- "url"
# 
# write.csv(revisions, 
#           "C:/Projects/Stack_Exchange/01_motivation_feedback/Answers/data/raw/urls_revisions_api.csv", 
#           row.names = FALSE)
# 
# rm(revisions)

# Check what we have already collected

# urls_revisions_api <- read.csv(file="./urls_revisions_api.csv", stringsAsFactors=FALSE)
revisions_info <- read.csv(file="./revisions_info.csv", 
                           encoding = "UTF-8", 
                           stringsAsFactors=FALSE)
revisions_info <- unique(revisions_info)

# urls_revisions_api <- urls_revisions_api %>% extract(col = url, into = "post_id",  
#                                                        regex = "posts/(\\d+)/", remove = FALSE)
# 
# urls_revisions_api <- urls_revisions_api %>%
#   filter(!(post_id %in% unique(revisions_info$post_id)))

# urls_revisions_api$post_id <- NULL
# 
# # update urls associated api with the missing requests
# write.csv(urls_revisions_api, "./urls_revisions_api.csv", row.names = FALSE)
# 
# rm(urls_revisions_api)

# COLLECT INITIAL BODY FOR THE ANSWER
keep <- c("post_id", "revision_number", "body")
revisions_info <- revisions_info[, which(names(revisions_info) %in% keep)]
revisions_info <- subset(revisions_info, revision_number == 1)

data_str_tr <- merge(data_str_tr, revisions_info[, c("post_id", "body")], 
                     by.x = "Id", by.y = "post_id", all.x = TRUE)

# missing body due to the answer being deleted
rm(revisions_info, keep)
# Remove users where first answer is deleted
data_str_tr <- subset(data_str_tr, !is.na(body))

# remove a new line
data_str_tr$WithoutHTML <- gsub("\n", " ", data_str_tr$body)
# remove what is inside code
data_str_tr$WithoutHTML <- gsub("<code>.*?</code>", "", data_str_tr$WithoutHTML)

# # get everything inside the html formatting
# data_str_tr$WithoutHTML <- gsub("<.*?>", "", data_str_tr$WithoutHTM)


# # UTF-8 General Punctuation
# # Initialize the dataframe and the row count
# HTML <- data.frame(Id = as.numeric(),
#                    content = as.character(),
#                    stringsAsFactors=FALSE)
# row = 1
# 
# for (i in seq(nrow(data_str_tr))) {
#   content <- str_match_all(data_str_tr$WithoutHTM[i], "(&.*?;)")[[1]][,2]
# 
#   if (length(content) != 0) {
#     for (n in seq(length(content))) {
#       HTML[row, 1] <- data_str_tr$Id[i]
#       HTML[row, 2] <- content[n]
#       row = row + 1
#     }
#   }
# }
# 
# HTML_freq <- data.frame(table(HTML$content))

# # remove UTF-8 general punctuation
# data_str_tr$WithoutHTML <- gsub("&.*?;", "", data_str_tr$WithoutHTML)
# data_str_tr$WithoutHTML <- gsub("\\s+", " ", str_trim(data_str_tr$WithoutHTML))

 
# # Initialize the dataframe and the row count
# HTML <- data.frame(Id = as.numeric(), 
#                    content = as.character(),
#                    stringsAsFactors=FALSE) 
# row = 1 
# 
# for (i in seq(nrow(data_str_tr))) {
#   content <- str_match_all(data_str_tr$body[i], "(<.*?>)")[[1]][,2]
#   
#   for (n in seq(length(content))) {
#     HTML[row, 1] <- data_str_tr$Id[i]
#     HTML[row, 2] <- content[n]
#     row = row + 1
#   }
# }
# 
# 
# 
# HTML <- data.frame(str_match_all(data_str_tr$body, "(<.*?>)")[[1]][, 2])
# colnames(HTML) <- "HTMLcontent"
# HTML_freq <- data.frame(table(HTML$content))

# # get everything inside remove a new line
# data_str_tr$BodyClean <- gsub("\n", " ", data_str_tr$body)
# # Remove html text and list formatting
# data_str_tr$BodyClean <- gsub("<(\\?|\\/?)(li|p|strong|em|ul|ol|pre|br|hr|h\\d|br|sup|sub|kbd|strike).*?>", 
#                                "", data_str_tr$BodyClean, ignore.case = TRUE)
# # </a>, </i>, </b>
# data_str_tr$BodyClean <- gsub("<\\/?(i|b)>", "", data_str_tr$BodyClean, ignore.case = TRUE)
# data_str_tr$BodyClean <- gsub("<\\/?(A)>", "", data_str_tr$BodyClean)

# # Initialize the dataframe and the row count
# HTML <- data.frame(Id = as.numeric(), 
#                    content = as.character(),
#                    stringsAsFactors=FALSE) 
# row = 1 
# 
# for (i in seq(nrow(data_str_tr))) {
#   content <- str_match_all(data_str_tr$BodyClean[i], "(<.*?>)")[[1]][,2]
# 
#   if (length(content) != 0) {
#     for (n in seq(length(content))) {
#       HTML[row, 1] <- data_str_tr$Id[i]
#       HTML[row, 2] <- content[n]
#       row = row + 1
#     }
#   }
# }
# 
# HTML_freq <- data.frame(table(HTML$content))

# #Alternative to BodyClean and WithoutHTML for counting words
# Convenience function to convert html codes
html2txt <- function(str) {
  xpathApply(htmlParse(str, encoding="UTF-8", asText=TRUE),
             "//body//text()",
             xmlValue)
}

 
# # Initialize the dataframe and the row count
# textWithoutHTML <- data.frame(Id = as.numeric(),
#                             content = as.character(),
#                             stringsAsFactors=FALSE)
# row = 1
# 
# 
# for (i in seq(nrow(data_str_tr))) {
#   text <- html2txt(data_str_tr$WithoutHTML[i])
# 
#   if (length(text) != 0) {
#     for (n in seq(length(text))) {
#       textWithoutHTML[row, 1] <- data_str_tr$Id[i]
#       textWithoutHTML[row, 2] <- text[n]
#       row = row + 1
#       }
#   } else {
#     textWithoutHTML[row, 1] <- data_str_tr$Id[i]
#     textWithoutHTML[row, 2] <- NA
#     row = row + 1
#   }
# }
# 
# # Initialize the dataframe and the row count
# textWithoutHTMLcollapse <- data.frame(Id = as.numeric(),
#                             bodyWithoutHTML = as.character(),
#                             stringsAsFactors=FALSE)
# row = 1
# 
# for (i in unique(textWithoutHTML$Id)) {
#   tmp <- subset(textWithoutHTML, Id == i)
# 
#   textWithoutHTMLcollapse[row, 1] <- i
#   textWithoutHTMLcollapse[row, 2] <- paste(tmp$content, collapse = "")
# 
#   row = row + 1
# 
# }
# 
# rm(textWithoutHTML, tmp, i, n, row, text)
# 
# write.csv(textWithoutHTMLcollapse,
#           "C:/Projects/Stack_Exchange/01_motivation_feedback/Answers/data/raw/textWithoutHTML.csv",
#           fileEncoding ="UTF-8",
#           row.names = FALSE)

data_str_tr$WithoutHTML <- NULL

textWithoutHTMLcollapse <- read.csv("C:/Projects/Stack_Exchange/01_motivation_feedback/Answers/data/raw/textWithoutHTML.csv",
                          fileEncoding ="UTF-8", 
                          stringsAsFactors=FALSE)

data_str_tr <- merge(data_str_tr, textWithoutHTMLcollapse, 
                     by="Id", all.x = TRUE)

rm(textWithoutHTMLcollapse)

# # Initialize the dataframe and the row count
# code_test <- data.frame(Id = as.numeric(),
#                             content = as.character(),
#                             stringsAsFactors=FALSE)
# row = 1
# 
# for (i in seq(nrow(data_str_tr))) {
#   
#   codes <- str_match_all(data_str_tr$Body[i], "(<code>.*?</code>)")[[1]][,2]
#   
#   if (length(codes) != 0) {
#     for (n in seq(length(codes))) {
#       
#       code_test[row, 1] <- data_str_tr$Id[i]
#       code_test[row, 2] <- codes[n]
#       row = row + 1
#       
#     }
#   }
# }

# Link to external resources + context (informed referenced answer)
# 3. % words that are linked to external resources
HtmlWords <- data.frame(Id = as.numeric(),
                            HTMLWordsCount = as.numeric(),
                            stringsAsFactors=FALSE)
row = 1

for (i in seq(nrow(data_str_tr))) {

  tmp <- str_match_all(data_str_tr$body[i], "<a href=.*?>(.*?)<\\/a>")[[1]][,2]
  
  if (length(tmp) != 0) {
    tmp <- html2txt(tmp) # remove html formatting and reference to <img>
    if (length(tmp) != 0) {
      tmp <- paste(tmp, collapse = "") # collapse if it is not already in one string
      tmp <- gsub("\n", " ", tmp)
      HtmlWords[row, 1] <- data_str_tr$Id[i]
      words <- strsplit(tmp, " ")[[1]] # remove empty/blank character string: ""
      HtmlWords[row, 2] <- length(words[words != ""])
      row = row + 1
    }
  }
}

data_str_tr <- merge(data_str_tr, HtmlWords, 
                     by="Id", all.x = TRUE)

rm(HtmlWords, i, row, tmp)

data_str_tr$HTMLWordsCount <- ifelse(is.na(data_str_tr$HTMLWordsCount), 0, data_str_tr$HTMLWordsCount)



# count the number of words referenced in a </blockquote>
RefsWords <- data.frame(Id = as.numeric(),
                        RefsWordsCount = as.numeric(),
                        stringsAsFactors=FALSE)
row = 1

for (i in seq(nrow(data_str_tr))) {
  
  tmp <- str_match_all(data_str_tr$body[i], "<blockquote>((.|\\s)*?)</blockquote>")[[1]][,2]
  if (length(tmp) != 0) {
    for (m in seq(length(tmp))){
      tmp[m] <- gsub("<code>(.*?)</code>", "", tmp[m])  # remove <code> frames
    }
    
    tmp <- html2txt(tmp) # remove html formatting and reference to <img>
    if (length(tmp) != 0) {
      tmp <- paste(tmp, collapse = "") # collapse if it is not already in one string
      tmp <- gsub("\n", " ", tmp)
      RefsWords[row, 1] <- data_str_tr$Id[i]
      words <- strsplit(tmp, " ")[[1]] # remove empty/blank character string: ""
      RefsWords[row, 2] <- length(words[words != ""])
      row = row + 1
    }
  }
}


data_str_tr <- merge(data_str_tr, RefsWords, 
                     by="Id", all.x = TRUE)

rm(RefsWords, i, row, tmp, m, words)

data_str_tr$RefsWordsCount <- ifelse(is.na(data_str_tr$RefsWordsCount), 0, data_str_tr$RefsWordsCount)


# Words Count
data_str_tr$WordsCount <- NA

for (i in seq(nrow(data_str_tr))) {
  words <- strsplit(data_str_tr$bodyWithoutHTML[i], " ")[[1]] # remove empty/blank character string: ""
  data_str_tr$WordsCount[i] <- length(words[words != ""])
  
}
rm(i, words)

# % words quoted and linked to external source
data_str_tr$externalSource_pct <- round(((data_str_tr$HTMLWordsCount + data_str_tr$RefsWordsCount)
                                          /data_str_tr$WordsCount)*100, 2)



# 4. Screenshots, mock-ups provided

# # Initialize the dataframe and the row count
# HTML <- data.frame(Id = as.numeric(),
#                    content = as.character(),
#                    stringsAsFactors=FALSE)
# row = 1
# 
# for (i in seq(nrow(data_str_tr))) {
#   content <- str_match_all(data_str_tr$BodyClean[i], "(<.*?>)")[[1]][,2]
# 
#   if (length(content) != 0) {
#     for (n in seq(length(content))) {
#       HTML[row, 1] <- data_str_tr$Id[i]
#       HTML[row, 2] <- content[n]
#       row = row + 1
#     }
#   }
# }
# 
# HTML_freq <- data.frame(table(HTML$content))

# Img Count 
data_str_tr$ImgCount <- NA

for (i in seq(nrow(data_str_tr))) {
  data_str_tr$ImgCount[i] <- length(str_match_all(data_str_tr$body[i], "(<img src=.*?/>)")[[1]][,2])
}

# Code Snippet Count 
data_str_tr$CodeCount <- NA

for (i in seq(nrow(data_str_tr))) {
  data_str_tr$CodeCount[i] <- length(str_match_all(data_str_tr$body[i], "(<code>.*?</code>)")[[1]][,2])
}

data_str_tr$Mockups <- data_str_tr$ImgCount + data_str_tr$CodeCount


# 5. Correct grammar 
# USE LANGUAGE TOOL PYTHON check for spelling and grammar mistakes
# textWithoutHTML <- data_str_tr[, which(names(data_str_tr) %in% c("Id", "WithoutHTML"))]
# write.csv(text,
#           "C:/Projects/Stack_Exchange/01_motivation_feedback/Answers/data/raw/textWithoutHTML.csv",
#           fileEncoding ="UTF-8",
#           row.names = FALSE)

grammar_spelling_analysis <- read.csv(file="./grammar_spelling_analysis_WithoutHTML.csv", 
                                      encoding = "UTF-8", 
                                      stringsAsFactors=FALSE)

disregard_punctuation <- c("COMMA_COMPOUND_SENTENCE_2", 
                           "COMMA_COMPOUND_SENTENCE", 
                           "PUNCTUATION_PARAGRAPH_END", 
                           "DOUBLE_PUNCTUATION", 
                           "EN_UNPAIRED_BRACKETS", 
                           "MISSING_COMMA_AFTER_INTRODUCTORY_PHRASE", 
                           "DASH_RULE", 
                           "COMMA_PERIOD", 
                           "PRP_COMMA", 
                           "MISSING_HYPHEN", 
                           "MISSING_COMMA_WITH_NNP", 
                           "YEAR_OLD_HYPHEN", 
                           "INTERJECTIONS_PUNCTUATION",
                           "NO_SPACE_CLOSING_QUOTE", 
                           "COMMA_THANKS", 
                           "COMMA_TAG_QUESTION", 
                           "BY_DEFAULT_COMMA", 
                           "OUT_OF_PLACE", 
                           "I_FOR_ONE_VB_COMMA", 
                           "FOR_NOUN_SAKE", 
                           "PRP_MD_PRP_MD_COMMA", 
                           "UNLIKELY_OPENING_PUNCTUATION", 
                           "ABBREVIATION_PUNCTUATION", 
                           "E_G",
                           "I_E", 
                           "UNIT_SPACE")

keep_punctuation <- c("SENT_START_CONJUNCTIVE_LINKING_ADVERB_COMMA",  
                      "QUESTION_MARK",  
                      "NO_COMMA_BEFORE_INDIRECT_QUESTION", 
                      "EG_SPACE", 
                      "APOS_ARE", 
                      "ENUMERATION_AND_DASHES", 
                      "COMMA_AFTER_A_MONTH",
                      "MISSING_COMMA_BETWEEN_DAY_AND_YEAR")


disregard_typos <- c("DISCUSSIONS_AROUND", 
                      "NOW", "A_BUT", "I_IF",
                      "MAY_BE", "APPSTORE",
                      "WONT_CONTRACTION", 
                      "COMPRISED_OF", 
                      "YOUR_YOU_2", 
                      "COULDVE_IRREGULAR_VERB",
                      "COMPARISONS_AS_ADJECTIVE_AS", 
                      "GOT_SHUTDOWN", 
                      "WORD_CONTAINS_UNDERSCORE", 
                      "ITS_JJ_NNSNN", 
                      "THE_HOW", 
                      "VB_A_WHILE", 
                      "IM_I_M", "APOSTROPHE_PLURAL", 
                      "HAY_DAY", "THOUGH_THROUGH", 
                      "IF_IS", "CAN_CHECKOUT", 
                      "BE_CAUSE", "IN_ANYWAY", 
                      "OTHER_THEN", 
                      "WHOS", "TR", "SAVE_SAFE", 
                      "ONES", "SCENT_SENT", 
                      "VBG_YOURE", 
                      "WHO_WHOM", "WERE_VBB", 
                      "WHOSE_DT", "APART_FORM")


keep_typos <- c("MORFOLOGIK_RULE_EN_US", # (accept British English, compare with the one found in the incorrect dict),)
                "I_LOWERCASE", "OUT_COME",  
                "IS_SHOULD", "IT_IS", # (there are some mistakes though)
                "MISSING_GENITIVE", 
                "POSSESSIVE_APOSTROPHE", "LAYS_ATOP", 
                "IM_AM", "EN_CONTRACTION_SPELLING",  
                "LETS_LET", "SETUP_VERB", 
                "NON_STANDARD_WORD", # (check with incorrect dictionary) 
                "ABOUT_ITS_NN", "ANY_MORE", 
                "COMPARISONS_THEN", 
                "AFTERALL", "IT_IS_2", 
                "EN_DIACRITICS_REPLACE", 
                "LIGHT_WEIGHT", "CONFUSION_OF_THEN_THAN", 
                "CANT", "FOR_AWHILE", 
                "WHERE_AS", "YOUR_SHOULD" ,
                "TEL_TELL", "THE_SOME_DAY", 
                "THROUGH_OUT", "ANY_BODY", 
                "META_DATA", "ALLOT_OF", 
                "TO_VB_ITS_NN", "MY_BE" , 
                "LOOSING_EFFORTRECORDSEASON",
                "TO_COMEBACK", "YOUR_YOU",
                "WORTH_WHILE", "BARE_IN_MIND",
                "WIFI", "IF_OF", "NOW_A_DAYS", 
                "INCORRECT_CONTRACTIONS", "WORLD_WIDE",
                "AM_LOATHE_TO", "CA_PRP",
                "FROM_FORM", "WHOS_NN", 
                "THEM_SELVES", "MI", "VERB_APOSTROPHE_S",
                "THEE", "THINK_OFF", "WHOM_WHO",
                "ITS_IS", "WITH_OUT", "ER", "CAN_BACKUP", 
                "THERE_FORE", "ET_AL", "ONE_OF_THE_ONLY",
                "TIS", "HASNT_IRREGULAR_VERB", "HE_THE",
                "ALONG_SIDE", "ANINFOR_EVERY_DAY", 
                "NEAR_BY", "KEY_WORDS", "OUT_SIDE",
                "PERS_PRON_CONTRACTION", "SENT_START_THEM",
                "STAND_ALONE_NN", "ONE_THE_ONE_HAND", 
                "ALONG_TIME", "OM", "FREE_REIGN",
                "FOR_FRO", "VARY_VERY", "APART_A_PART",
                "AN_THEN", "ELUDED_TO", "NEED_TO_VBG",
                "SOME_WHAT_JJ", "BUILD_OFF_OF", "ANY_WHERE",
                "MODAL_OF", "LOT_S", "INCASE_OF", 
                "HOW_EVER","ONE_ORE")
                

disregard_typography <- c("SENTENCE_WHITESPACE", "EN_QUOTES", 
                          "WHITESPACE_RULE", "MULTIPLICATION_SIGN",
                          "COMMA_PARENTHESIS_WHITESPACE", 
                          "ARROWS", "APOSTROPHE_IN_DATES", 
                          "CURRENCY", "PLUS_MINUS", "TRADEMARK", 
                          "TL_DR", "APOS_SPACE_CONTRACTION", 
                          "HYPOTHESIS_TYPOGRAPHY", 
                          "COPYRIGHT", "APOSTROPHE_VS_QUOTE", 
                          "WRONG_APOSTROPHE")


keep_typography <- c("SPURIOUS_APOSTROPHE", "APOSTROPHE_IN_DAYS")


keep_confused <- c("LESS_MORE_THEN",  "DOSE_DOES", 
                   "TO_TOO", # (some mistakes) 
                   "INSURE_THAT",  "COMMA_THAN", # (some mistakes)
                   "KNOW_NOW", # (some mistakes)
                   "MAY_MANY", "THERE_THEIR",
                   "DO_TO", "THERE_OWN", "YOUR_NN",
                   "LOSE_LOSS", "TOO_TO", 
                   "NO_NOT", "THE_LATER_LATTER",
                   "YOUR", "SOMETIME_SOMETIMES",
                   "ASK_WETHER", "THEY_WHERE",
                   "AND_THAN", "TAT", "ARCHITECT_VERB",
                   "AFFECT_EFFECT", "YO_TO",
                   "CONFUSION_OF_MANS_MEN", "CLEAN_UP",
                   "RATHER_THEN", "ADJECTIVE_ADVERB",
                   "THEIR_IS", "CALENDER",
                   "AN_AND", "INTENT_INTEND",
                   "COULD_OF", "ASSES_ASSESS",
                   "DELIVERY_DELIVER", "THING_THINK",
                   "IN_PRINCIPAL", "THAT_THAN", 
                   "WORK_AS_A_CHARM", "DEPENDENT",
                   "WHAT_IT_HAPPENING", "FORE_FOR",
                   "TRAIL_TRIAL", "PRECEDENT_PRECEDENCE",
                   "BACK_IN_FORTH",  "TROUGH_THROUGH", 
                   "IT_SEAMS", "GIVE_ADVISE", "ANS_AND", 
                   "CONFUSION_OF_SATE_STATE", "BACK_IN_FORTH",
                   "TROUGH_THROUGH", "IT_SEAMS",
                   "GIVE_ADVISE", "ANS_AND",
                   "CONFUSION_OF_SATE_STATE", 
                   "QUIET_QUITE", "BESIDES_BESIDE",
                   "WAN_WANT", "WHAT_IT_THE", "ADVICE_ADVISE",
                   "BLU_RAY", "MAY_MANY_MY", 
                   "LOOSE_LOSE", "SINCE_FOR", "NUMBER_OF_NNS")

disregard_confused <- c("RED_READ", 
                        "NO_KNOW", "LOG_IN", 
                        "TO_TWO", "EVERYDAY_EVERY_DAY", 
                        "IS_US", "VBG_THEYRE", 
                        "WERE_WHERE", "DUN_DONT", 
                        "CONFUSION_DUE_DO", 
                        "PERSONAL_PERSONNEL", "BY_BUY", 
                        "ECONOMICAL_ECONOMIC")


keep_grammar <- c("CA_FOLLOW_UP", "MD_BASEFORM", "DOSNT",
                  "THERE_S_MANY", "IT_VBZ", "MOST_COMPARATIVE",
                  "TWITTER", "HAVE_PART_AGREEMENT", "A_NNS",
                  "ON_SKYPE", "ADMIT_ENJOY_VB", #(some errors)
                  "PLURAL_VERB_AFTER_THIS", "HOLDER_COMPOUNDS",
                  "PERS_PRONOUN_AGREEMENT", "IN_WHO",
                  "MCDONALDS", "MISSING_TO_BEFORE_A_VERB", #(some errors) 
                  "HE_VERB_AGR", "MOST_SOME_OF_NNS",
                  "ADVISE_VBG", "WILL_BASED_ON", 
                  "BEEN_PART_AGREEMENT", "BUILT_IN_HYPHEN",
                  "WAY_COMPOUNDS", "DT_DT", "NON3PRS_VERB",
                  "THE_THEY", "DEPEND_ON", "MOST_OF_THE_TIMES",
                  "IF_VB_PCT", "ALL_NN", "MICROSOFT_PRODUCTS",
                  "HE_D_VBD","THERE_VBP_NN", "EVERY_EACH_SINGULAR",
                  "THE_SAME_AS", "THIS_MISSING_VERB",
                  "MAN_COMPOUNDS", "FEWER_LESS", "A_GOOGLE",
                  "PRP_HAVE_VB", "BUNCH_OF", "PLACE_COMPOUNDS",
                  "AFTERMARKET", "MD_BE_NON_VBP",
                  "A_INFORMATION", "PRP_VBG", "THE_WORSE_OF", 
                  "EVER_NN", "PRINT_COMPOUNDS",
                  "ACCORDING_TO_ME", "KIND_OF_A",
                  "HOUSE_COMPOUNDS", "COMP_THAN", "WOLD_WOULD",
                  "ALL_MOST_SOME_OF_NOUN", "AM_I", "SOME_FACULTY",
                  "YOUR_RE", "THEIR_S", "U_RE", "MANY_NN", 
                  "AGREEMENT_SENT_START",
                  "AUXILIARY_DO_WITH_INCORRECT_VERB_FORM",
                  "LOTS_OF_NN", "THIS_NNS", "NON_ACTION_CONTINUOUS",
                  "DOUBLE_NEGATIVE", "COPY_PASTE",
                  "AFFORD_VBG", "AFFORD_VBG", "THERE_RE_MANY",
                  "A_RB_NN", "RELATIVE_CLAUSE_AGREEMENT",
                  "AFFORD_VBG", "AFFORD_VBG", "THERE_RE_MANY",
                  "A_RB_NN", "RELATIVE_CLAUSE_AGREEMENT",
                  "TO_NON_BASE", "YOU_TUBE", "SOFTWARES",
                  "NEITHER_NOR",  "MUCH_COUNTABLE", "OVER_COMPOUNDS",
                  "LINE_COMPOUNDS", "CONDITIONAL_CLAUSE", "PRP_RB_JJ",
                  "DOES_X_HAS", "AND_END", "RELY_ON", "A_UNCOUNTABLE",
                  "THERE_MISSING_VERB", "BREAKER_COMPOUNDS",
                  "SUPPOSE_TO", "A_BIT", "CD_NN", "LIFE_COMPOUNDS",
                  "MORE_A_JJ", "SINGULAR_VERB_AFTER_THESE_OR_THOSE",
                  "EXPLAIN_TO", "A_LOT_OF_NN", "MANY_NN_U",
                  "THERE_WAS_MANY", "SINGULAR_AGREEMENT_SENT_START",
                  "IN_THE_MEAN_TIME_PHRASE", "IN_TERM_OF_PHRASE", 
                  "SIGN_INTO", "PRP_PAST_PART", "ALLOW_TO_DO",
                  "COUNTER_COMPOUNDS", "DID_BASEFORM", "PROGRESSIVE_VERBS",
                  "PEOPLE_VBZ", "THE_MOST", "HOMO_SAPIENS", "TO_RB_TO_VB",
                  "UP_COMPOUNDS",  "PRP_HAVES",  "BOTH_AS_WELL_AS",
                  "DID_FOUND_AMBIGUOUS", "DOES_NP_VBZ", "WHAT_IS_YOU",
                  "SUPERIOR_THAN_TO", "THE_ARE", "PRP_DON",
                  "BASE_FORM", "ONE_PLURAL", "ON_COMPOUNDS",
                  "HAPPY_EASTER", "DID_PAST", "MISSING_ARTICLE", 
                  "NOTE_COMPOUNDS", "SOME_NN_VBP", "GAMEBOY",
                  "HE_NEED", "I_MA", "IN_WEEKDAY", "BE_VBP_IN",
                  "AFFORD_VB", "IF_THERE", "AGREEMENT_SENT_START_2",
                  "EQUALLY_AS", "PRP_FOND", "BACK_COMPOUNDS",
                  "THE_BEST_WAY", "CROSS_COMPOUNDS",
                  "PRP_RB_NO_VB", "HAD_VBP", "A_CD_NNS",
                  "ALLOW_TO","BE_NO_VB", "BE_NO_VB", "A_BIT_OF",
                  "SPLITTED", "ON_FIRST_GLANCE", "AN_ANOTHER",
                  "SUPERLATIVE_THAN", "CAUSE_BECAUSE", "DIE_DICE",
                  "EYE_COMPOUNDS", "STRIKED", 
                  "THANK_FULL", "TWO_CONNECTED_MODAL_VERBS", "IS_VBZ",
                  "THERE_AFTER", "MATE_COMPOUNDS", "BE_TO_VBG",
                  "MANY_TIME", "PROOF_COMPOUNDS", 
                  "FURTHER_MORE_COMPOUND", "HARDWARES",
                  "THESE_ONES", "IT_ITS", "AS_ADJ_AS", "DOESENT",
                  "TAKEAWAY", "SHORT_COMPARATIVES",  "REGARDING_TO",
                  "HEAD_COMPOUNDS", "DO_HE_VERB", 
                  "DETERMINER_GEOGRAPHICAL_WORD",  "ABLE_VBP",
                  "WORK_AROUND_COMPOUND", "DOWN_COMPOUNDS", 
                  "WOULD_BE_JJ_VB", "POSSESSIVE_CASE", "WHAT_TO_VBD", 
                  "TOO_EITHER", "DT_RB_IN", "THIS_NNS_VB",
                  "SHIP_COMPOUNDS", "PRP_NO_VB", 
                  "THE_EXACTLY_THE", "BELIVE_BELIEVE", 
                  "MOST_SUPERLATIVE", "THIS_TOOLS",
                  "I_NOT_JJ", "DO_VBZ", "BE_INTEREST_IN", 
                  "AN_VB_PRP", #(with mistakes)
                  "IT_IT", "A_MD_VB", "A_COMPLAIN")


disregard_grammar <- c("A_INFINITIVE", "HEADED_HYPHEN",
                       "DT_PRP", # due to deleting html formatting 
                       "SENTENCE_FRAGMENT",
                       "PRP_VB", "ADVERB_WORD_ORDER", 
                       "SIMPLE_TO_USE_HYPHEN", 
                       "PRP_JJ", "FACTOR_HYPHEN", 
                       "COLLECTIVE_NOUN_VERB_AGREEMENT_VBD", 
                       "DOUBLE_CLICK_HYPHEN", 
                       "ANYMORE_ADVERB", "NEWS_COMPOUNDS", 
                       "A_WINDOWS", "SUBJECT_MATTER_HYPHEN",
                       "MISSING_TO_BETWEEN_BE_AND_VB", 
                       "A_ATTACH", "THE_SUPERLATIVE", 
                       "ADVERB_VERB_ADVERB_REPETITION", 
                       "STARS_AND_STEPS", "THE_SENT_END", 
                       "NODT_DOZEN", "TO_DO_HYPHEN", 
                       "IF_WOULD_HAVE_VBN", 
                       "PHRASE_REPETITION", # repetitions sometimes are used as an example
                       "SHOULD_BE_DO", "DOUBLE_HYPHEN", 
                       "MASS_AGREEMENT", 
                       "A_INSTALL", "PREPOSITION_VERB",
                       "EACH_EVERY_NNS", "A_TO", "ON_EXCEL", 
                       "MAKE_UP","USE_TO_VERB", 
                       "LOGGED_IN_HYPHEN", "CLICK_HYPHEN",  
                       "PRP_MD_NN",  "THE_PUNCT",  
                       "APPLE_PRODUCTS", "WORD_ESSAY_HYPHEN", 
                       "OBJECTIVE_CASE", "RUNNER_UP_HYPHEN", 
                       "MUST_HAVE","PRP_THE", "TOO_ADJECTIVE_TO",
                       "STEP_BY_STEP_HYPHEN", 
                       "DT_JJ_NO_NOUN", "GO_TO_HYPHEN",
                       "POKEMON", "NO_GO_HYPHEN", "A_THANK_YOU", 
                       "WEEK_LONG_HYPHEN", "DEGREE_HYPHEN", 
                       "OPT_IN_HYPHEN",  "CALL_TO_ACTION_HYPHEN", 
                       "MARKDOWN_NNP", "CA_BRAND_NEW", "HAVE_TWITTER", 
                       "TOP_DOWN_HYPHEN", "TEN_FOLD", 
                       "OPEN_OFFICE", "SET_TOP_BOX_HYPHEN", 
                       "ADVERB_OR_HYPHENATED_ADJECTIVE",
                       "MONEY_BACK_HYPHEN", "THE_HOT_DOG", 
                       "SOON_TO_BE_HYPHEN", "HIGH_END_HYPHEN", 
                       "THOUGH_THOUGHT",  "CLEAR_CUT_HYPHEN", 
                       "THE_NN_AND_THE_NN", "HOUR_HYPHEN",
                       "ARTICLE_ADJECTIVE_OF", "MUST_BE_DO", 
                       "MISSING_NOUN", "YEAR_HYPHEN",  
                       "SO_CALLED_HYPHEN", "MISSING_PAST_TENSE",
                       "A_COLLECTIVE_OF_NN", "PERSON_HYPHEN",
                       "NEEDNT_TO_DO_AND_DONT_NEED_DO", 
                       "CATCH_ALL_HYPHEN", "MUCH_NEEDED_HYPHEN", 
                       "DAY_TO_DAY_HYPHEN", "PROBLEM_SOLVE_HYPHEN",
                       "HAD_TOO", "GOOGLE_PRODUCTS", 
                       "COLLECTIVE_NOUN_VERB_AGREEMENT_VBP", 
                       "INCORRECT_POSSESSIVE_FORM_AFTER_A_NUMBER")


keep_misc <- c("EN_A_VS_AN", "EN_COMPOUNDS")
disregard_misc <- c("ENGLISH_WORD_REPEAT_RULE", "EN_WORD_COHERENCY")

keep_casing <- c("EN_SPECIFIC_CASE")
disregard_casing <- c("UPPERCASE_SENTENCE_START", 
                         "LINKEDIN", "PAYPAL", 
                         "YOUTUBE", "LC_AFTER_PERIOD", 
                         "THE_FRENCH", "GITHUB", "WORDPRESS")

keep_collocations <- c("ON_IN_THE_AFTERNOON", "INDEPENDENTLY_FROM_OF", 
                       "RETURN_IN_THE", "ARRIVAL_TO_THE_HOUSE",
                       "BY_EXAMPLE", "ON_IN_THE_CORNER",
                       "HIDE_OF_FROM", "ENTER_IN", "IN_AT_THE_TOP",
                       "IN_ON_THE_RIGHT_HAND_SIDE", "GOOD_IN_AT_GERUND",
                       "IT_IS_SURE", "ACCUSTOM_WITH_TO",
                       "ASK_TO", "IN_THE_INTERNET", "WORRY_FOR",
                       "ARRIVE_ON_AT_THE_BEACH", "LOT_OF",
                       "ATD_VERBS_TO_COLLOCATION","A_MY",
                       "CONSEQUENCES_OF_FOR", "ACCOMPANY_WITH",
                       "INTEREST_ABOUT_IN", "FULL_WITH_OF", "DAMAGE_OF_TO")

disregard_collocations <- c("NON_ANTI_JJ", "MENTION_ABOUT", "MISSING_PREPOSITION",
                            "ADVERTISEMENT_OF_FOR","IN_FACEBOOK")

keep_nostd <- c("IN_OR_WITH_REGARDS_TO_OF",  "INFORMATIONS",  "THE_QUESTION_WH")


# we don't consider errors the following 
grammar_spelling_analysis <- subset(grammar_spelling_analysis, 
                                    !(category %in% c("REDUNDANCY", "SEMANTICS",
                                                      "BRITISH_ENGLISH", "STYLE", 
                                                      "AMERICAN_ENGLISH_STYLE", 
                                                      "COMPOUNDING")))

# remove COMMA_PARENTHESIS_WHITESPACE could be due to the previous text cleaning steps
grammar_spelling_analysis <- subset(grammar_spelling_analysis, 
                                    !(ruleId %in% c(disregard_punctuation, 
                                                    disregard_typos, 
                                                    disregard_typography,
                                                    disregard_confused, 
                                                    disregard_grammar, 
                                                    disregard_misc, 
                                                    disregard_casing, 
                                                    disregard_collocations)))


# # TODO -  CHECK THE COMMENTS ABOVE
# # remove messages that refers to British English
# # clean up using the message column
# tmp_str = "Possible spelling mistake. ‘optimise’ is British English."
# substr(tmp_str, nchar(tmp_str) - 15, nchar(tmp_str)) != "British English."
# > foo <- subset(typos, ruleId == "MORFOLOGIK_RULE_EN_US")
# > foo <- subset(typos, substr(message, nchar(message) - 15, nchar(message)) != "British English.")

# NB.:                                                  
# STYLE: take out from grammar, but it could be used to profile the users (informal, offensive, etc. )












