library(plyr)
library(dplyr)
library(tidyr)
library(stringr)
library(hunspell)
library(textutils)

# Import file

setwd("C:/Projects/Stack_Exchange/01_motivation_feedback/Answers/data")
data_str_tr <- read.csv("data_str_tr_04_gt_ft.csv", stringsAsFactors = FALSE)

# ** BACKGROUND INFORMATION **
# Would good contributors (in terms of their ability and motivations)
# continue to contribute independently from the feedback received?
# Exploit the serendipitous variation in the dataset.
# Those unexpected patterns, such as a good contributor that gets a negative 
# feedback/no feedback and continue to contribute nevertheless (reducing selection bias). 
# 
# The problem could be narrowed down in identifying good contributors (and vice versa)
# through the scoring of the following aspects 
# (possibly calculating them on the first answer given and on the Initial Body):
#   
# -	Changes the author made on their own answers (details added - further research in the topic)
# -	Answer given on their own question (even if they didn't get the answer they were looking 
#                                     for they come back to the website to share the newly acquire 
#                                     knowledge for future users)
# -	Link to external resources + context (informed referenced answer)
# -	Screenshots, mock-ups provided
# •	Correct grammar 
# •	Reputation (asked multiple answers and now it is time to give back at the community)

# •	Greetings, exclamation marks, question marks, etc.. (signs of poor answer)
# •	Answer was flag as offensive, duplicate, etc.. Or deleted due to malpractice 
#   Which hopefully could help us in identifying the following:
#     o	Lack of full/comprehensive explanation
#     o	Lack of focus and clarity 
#     o	Opinions/discussion based answers

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
rm(revisions_info)

# get everything inside remove a new line
data_str_tr$WithoutHTML <- gsub("\n", " ", data_str_tr$body)
data_str_tr$WithoutHTML <- gsub("<code>.*?</code>", " ", data_str_tr$WithoutHTML)

# get everything inside <.*>
data_str_tr$WithoutHTML <- gsub("<.*?>", "", data_str_tr$WithoutHTM)


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

# remove UTF-8 general punctuation
data_str_tr$WithoutHTML <- gsub("&.*?;", "", data_str_tr$WithoutHTML)
data_str_tr$WithoutHTML <- gsub("\\s+", " ", str_trim(data_str_tr$WithoutHTML))

 
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

# get everything inside remove a new line
data_str_tr$BodyClean <- gsub("\n", " ", data_str_tr$body)
# Remove html text and list formatting
data_str_tr$BodyClean <- gsub("<(\\?|\\/?)(li|p|strong|em|ul|ol|pre|br|hr|h\\d|br|sup|sub|kbd|strike).*?>", 
                               "", data_str_tr$BodyClean, ignore.case = TRUE)
# </a>, </i>, </b>
data_str_tr$BodyClean <- gsub("<\\/?(i|b)>", "", data_str_tr$BodyClean, ignore.case = TRUE)
data_str_tr$BodyClean <- gsub("<\\/?(A)>", "", data_str_tr$BodyClean)

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

# Link to external resources + context (informed referenced answer)
# count the number of links in a answer
data_str_tr$CountHtml <- NA

for (i in seq(nrow(data_str_tr))) {
  data_str_tr$CountHtml[i] <- length(str_match_all(data_str_tr$BodyClean[i], "(<a href=.*?>)")[[1]][,2])
}

# TODO % worlds that are linked to external resources
# foo <- subset(data_str_tr, Id == 15741)
# foo <- subset(data_str_tr, Id == 15749)
# foo <- subset(data_str_tr, Id == 119596)
data_str_tr$CountWordsHtml <- NA

for (i in seq(nrow(data_str_tr))) {
  HtmlContent <- str_match_all(data_str_tr$BodyClean[i], "(<a href=.*?>(.*?)</a>)")[[1]][,3]
  HtmlContent <- paste(HtmlContent, collapse = " ")
  
  # take out img referenced inside a href
  HtmlContent <- gsub("<img src=.*?/>", "", HtmlContent, ignore.case = TRUE)
  HtmlContent <- gsub("\\s+", " ", str_trim(HtmlContent))

  data_str_tr$CountWordsHtml[i] <- sapply(strsplit(HtmlContent, " "), length)
}

# count the number of </blockquote>/reference in a 
data_str_tr$CountRef <- NA

for (i in seq(nrow(data_str_tr))) {
  data_str_tr$CountRef[i] <- length(str_match_all(data_str_tr$BodyClean[i], "(<blockquote>.*?</blockquote>)")[[1]][,2])
}

# TODO % worlds that are quoted
# foo <-subset(data_str_tr, Id == 95542
# foo <-subset(data_str_tr, Id == 74935)

data_str_tr$CountWordsRef <- NA

for (i in seq(nrow(data_str_tr))) {
  RefContent <- str_match_all(data_str_tr$BodyClean[i], "<blockquote>(.*?)</blockquote>")[[1]][,2]
  RefContent <- paste(RefContent, collapse = " ")
  RefContent <- gsub("\\s+", " ", str_trim(RefContent))
  data_str_tr$CountWordsRef[i] <- sapply(strsplit(RefContent,  " "), length)
}

# TODO (check again) Words Count
data_str_tr$WordsCount <- NA

for (i in seq(nrow(data_str_tr))) {
  data_str_tr$WordsCount[i] <- sapply(strsplit(data_str_tr[i, 'WithoutHTML'], " "), length)
}

# TODO % words quoted & % of words linked to external source
# 3 Screenshots, mock-ups provided

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
  data_str_tr$ImgCount[i] <- length(str_match_all(data_str_tr$BodyClean[i], "(<img src=.*?/>)")[[1]][,2])
}

# Code Snippet Count 
data_str_tr$CodeCount <- NA

for (i in seq(nrow(data_str_tr))) {
  data_str_tr$CodeCount[i] <- length(str_match_all(data_str_tr$BodyClean[i], "(<code>.*?</code>)")[[1]][,2])
}

data_str_tr$mockups <- data_str_tr$ImgCount + data_str_tr$CodeCount


# 4. Correct grammar + update current dictionary
## PS.: step no needed in the LanguageTool in python
# check for spelling
# remove email address and external links from WithoutHTML
for (i in seq(nrow(data_str_tr))) {
  if (!is.na(data_str_tr$WithoutHTML[i])) {
    data_str_tr$WithoutHTML[i] <- gsub("(http|https|www)(:|\\.).[^ ]*", " ", 
                                       data_str_tr$WithoutHTML[i]) # remove external links
    data_str_tr$WithoutHTML[i] <- gsub(".[^ ]*@.[^ ]*", " ", 
                                       data_str_tr$WithoutHTML[i]) # remove email address
    data_str_tr$WithoutHTML[i] <- gsub("\\d+.[^ ]*", " ", 
                                       data_str_tr$WithoutHTML[i]) # remove words that starts with numbers
    # data_str_tr$WithoutHTML[i] <- gsub("\\w+[[:punct:]]\\w+", " ", 
    #                                    data_str_tr$WithoutHTML[i])# remove concatenation of words that contains punctuation
    data_str_tr$WithoutHTML[i] <- gsub("\\s+", " ", str_trim(data_str_tr$WithoutHTML[i]))
    data_str_tr$WithoutHTML[i] <- gsub("(\\s+)(\\W)", "\\2 ", str_trim(data_str_tr$WithoutHTML[i])) # remove spaces before punctuation
    
  }
}


#https://predictivehacks.com/languagetool-grammar-and-spell-checker-in-python/
# build up the community dictionary
dict <- dictionary(add_words = c("en_GB-ise", "en_GB-ize", "affordance", "dropdown", "html",
                         "colour", "colours", "px", "tooltip", "tooltips", 
                         "behaviour", "ux", "checkboxes", "eg", "grey",
                         "wireframes", "analytics", "nav", "png", "stackexchange",
                         "imgur", "mockup", "wikipedia", "css", "ie",
                         "jQuery", "facebook", "io", "javascript",
                         "signup", "wireframe", "jpg", "url", "mockups", 
                         "ok", "scrollbar", "textbox", "dropdowns", "viewport",
                         "dialogs", "autocomplete", "backend", "webpage", 
                         "ecommerce", "dev", "scrollable", "whitespace", 
                         "bmml", "onboarding", "pre", "un", "greyed", 
                         "gmail", "ui", "captcha", "de", "js", "checkmark", 
                         "english", "photoshop", "ajax", "ipsum", "jquery", 
                         "eCommerce",  "dataset", "discoverability", "gamification", 
                         "lightbox", "pdf", "mouseover", "selectable",
                         "labelled", "upvote", "et", "gui",
                         "coloured", "organisation", "realise", "youtube",  
                         "favourite", "gif", "geolocation", "recognise", 
                         "wireframing", "frontend", "se", "stackoverflow",
                         "textarea", "webapp", "al", "centre", "combobox", 
                         "mis", "programmatically", "scannable", "gameplay",
                         "amongst", "boolean", "ctrl",
                         "scalable", "customizable", "discoverable", "german", "", 
                         "lorem", "recognisable", "signifier", "unicode", 
                         "unselected", "datepicker", "minimise", "navbar", 
                         "radiobuttons", "unintuitive", "walkthrough", "", 
                         "api", "autosave", "bolded", "cancelling", "deliverables",
                         "deliverable", "draggable", "fullscreen", "github", 
                         "greyscale", "infographic", "iphone", 
                         "monospaced", "numpad", "readonly", "recognised", 
                         "skeuomorphic", "tappable", "trackpad", "userbase", 
                         "autoplay", "balsamiq", "config", "facto",
                         "iframe", "infographics", "labelling", "os", "practise",
                         "prepend", "prioritise", "spacebar", "touchpad", 
                         "tradeoff", "analyse", "async", "copywriting", 
                         "drilldown", "ebay", "favour", "firefox", "glyphs",
                         "gridlines", "iconization", "ip", "ipad", "", "",
                         "mailto", "realised", "outro", "subpages", 
                         "substring", "tickbox", "toolset", "typeahead",
                         "visualisation", "walkthroughs", "wishlist", 
                         "autofill", "blogpost", "callout", "callouts",
                         "catalogue", "collapsable", "commenter", 
                         "commenters", "dialling", "dropbox", "durations", 
                         "esc", "expander", "feedbacks", "focussed", "folksonomy",
                         "heatmap", "homescreen", "http", "linkedin", "listbox",
                         "modelling", "moveable", "navigations", "pc", "tv",
                         "roadmap", "skeuomorphism", "sortable", "specialised", 
                         "standardised",
                         "subforum", "useable", "utilise", "visualise", 
                         "breakpoint", "cancelled", "categorisation", "chatrooms",
                         "chatroom", "craigslist", "csv", "customise", 
                         "customised", "dependant", "desaturated", "downvote",
                         "favorited", "gamepad", "gamers", "grippy", 
                         "incrementing", "judgement", "learnt",
                         "lifecycle", "macbook", "multitouch", "optimise",
                         "organised", "php", "preselected", "pushback", "quickviews", 
                         "smilies", "sparklines", "usb", "xbox", "timesheet", 
                         "softwares", "spanish", "walkman", "submenu", "timeslots",
                         "transactional", "xp"))




hunspell_suggest("")

# Initialize the dataframe and the row count
incorrect_dict <- data.frame(Id = as.numeric(),
                   content = as.character(),
                   stringsAsFactors=FALSE)
row = 1


for (i in seq(nrow(data_str_tr))) {
  if (!is.na(data_str_tr$WithoutHTML[i])) {
    
    tokens <- hunspell_parse(data_str_tr$WithoutHTML[i], format = "latex", dict=dict)
    tokens <- grep("^[A-Z]", tokens[[1]], invert = TRUE, value = TRUE) # remove all capital letters words (proper names)
    tokens <- grep("(\\w)\\1{2,}", tokens, invert = TRUE, value = TRUE) # remove single letters words greater than two syllabi
    tokens <- grep("\\w*?[A-Z]\\w*?", tokens, invert = TRUE, value = TRUE) # remove word if it contains an apper letter
    
    correct <- hunspell_check(tokens, dict = dict)
    incorrect <- tokens[!correct]
    
    if (length(incorrect) != 0) {
      for (n in seq(length(incorrect))) {
        incorrect_dict[row, 1] <- data_str_tr$Id[i]
        incorrect_dict[row, 2] <- incorrect[n]
        row = row + 1
      }
    }
  }
}


incorrect_freq <- data.frame(table(incorrect_dict$content))
incorrect_freq <- subset(incorrect_freq, Freq > 3)

# # USE LANGUAGE TOOL PYTHON check for spelling and grammar mistakes
# text <- data_str_tr[, which(names(data_str_tr) %in% c("Id", "WithoutHTML"))]
# write.csv(text,
#           "C:/Projects/Stack_Exchange/01_motivation_feedback/Answers/data/raw/text.csv",
#           fileEncoding ="UTF-8", 
#           row.names = FALSE)

grammar_spelling_analysis <- read.csv(file="./grammar_spelling_analysis.csv", 
                                       encoding = "UTF-8", 
                                       stringsAsFactors=FALSE)



# remove COMMA_PARENTHESIS_WHITESPACE could be due to the previous text cleaning steps
grammar_spelling_analysis <- subset(grammar_spelling_analysis, 
                                    !(ruleId %in% c("COMMA_PARENTHESIS_WHITESPACE", 
                                                    "SENTENCE_WHITESPACE",
                                                    "WHITESPACE_RULE", 
                                                    "EN_QUOTES", 
                                                    "MULTIPLICATION_SIGN", 
                                                    "PLUS_MINUS", 
                                                    "TRADEMARK", 
                                                    "APOS_SPACE_CONTRACTION", 
                                                    "TL_DR", 
                                                    "APOSTROPHE_IN_DAYS", 
                                                    "COPYRIGHT", 
                                                    "COMMA_COMPOUND_SENTENCE_2", 
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
                                                    "OUT_OF_THE_WAY", 
                                                    "OUT_OF_PLACE", 
                                                    "I_FOR_ONE_VB_COMMA", 
                                                    "FOR_NOUN_SAKE", 
                                                    "PRP_MD_PRP_MD_COMMA", 
                                                    "UNLIKELY_OPENING_PUNCTUATION", 
                                                    "ABBREVIATION_PUNCTUATION"
                                                    )))

# Find the relevant category and rule id
length(unique(grammar_spelling_analysis$ruleId))
length(unique(grammar_spelling_analysis$category))

typography <- subset(grammar_spelling_analysis, category == "TYPOS")
unique(typography$ruleId)
foo <- subset(typography, ruleId == "VBZ_VBD" )
unique(foo$message)


grammar_spelling_analysis <- subset(grammar_spelling_analysis, 
                                    !(ruleId %in% c("DISCUSSIONS_AROUND", 
                                                    "NOW", 
                                                    "A_BUT", 
                                                    "I_IF", 
                                                    "YOURS_APOSTROPHE", 
                                                    "MAY_BE", 
                                                    "WONT_CONTRACTION", 
                                                    "COMPRISED_OF", 
                                                    "APPSTORE", 
                                                    "YOUR_YOU_2", 
                                                    "COULDVE_IRREGULAR_VERB",
                                                    "COMPARISONS_AS_ADJECTIVE_AS", 
                                                    "GOT_SHUTDOWN", 
                                                    "WORD_CONTAINS_UNDERSCORE", 
                                                    "ITS_JJ_NNSNN", 
                                                    "THE_HOW", 
                                                    "VB_A_WHILE", 
                                                    "IM_I_M", "APOSTROPHE_PLURAL", 
                                                    "CONFUSION_OF_FOND_FUND", 
                                                    "HAY_DAY", "THOUGH_THROUGH", 
                                                    "IF_IS", "CAN_CHECKOUT", 
                                                    "BE_CAUSE", "IN_ANYWAY", 
                                                    "AS_FOLLOW", "OTHER_THEN", 
                                                    "WHOS", "TR", "SAVE_SAFE", 
                                                    "ONES", "SCENT_SENT", 
                                                    "TOO_DETERMINER", "HELL", 
                                                    "VBG_YOURE", "WHO_WHOM", 
                                                    "WHOSE_DT", "APART_FORM", 
                                                    "WERE_VBB", "VBZ_VBD")))
                                            

bar = subset(data_str_tr, Id == 84594)
bar$body
bar$WithoutHTML
substr(bar$WithoutHTML, 20, 40)
subset(incorrect_dict, Id == 24673)




category <- subset(grammar_spelling_analysis, category == "COLLOCATIONS")
#ruleIssueType <- subset(category, ruleIssueType == 'misspelling')
unique(category$ruleId)
ruleId <- subset(category, ruleId == )
unique(ruleId$message)

# "COLLOCATIONS": "ON_IN_THE_AFTERNOON" "INDEPENDENTLY_FROM_OF" "RETURN_IN_THE" "ARRIVAL_TO_THE_HOUSE"
#                 "BY_EXAMPLE" "ON_IN_THE_CORNER" "HIDE_OF_FROM" "ENTER_IN" "IN_AT_THE_TOP"
#                 "IN_ON_THE_RIGHT_HAND_SIDE" "GOOD_IN_AT_GERUND" "IT_IS_SURE" "ACCUSTOM_WITH_TO"
#                 "ASK_TO" "IN_THE_INTERNET" "WORRY_FOR" "ARRIVE_ON_AT_THE_BEACH" "LOT_OF"
#                 "ATD_VERBS_TO_COLLOCATION"   "CONSEQUENCES_OF_FOR" "ACCOMPANY_WITH" "INTEREST_ABOUT_IN"
#                 "FULL_WITH_OF" "DAMAGE_OF_TO"



# "GRAMMAR": "CA_FOLLOW_UP" "MD_BASEFORM" "DOSNT" "THERE_S_MANY" "IT_VBZ" "MOST_COMPARATIVE"
#            "TWITTER" "HAVE_PART_AGREEMENT" "A_NNS" "ON_SKYPE" "ADMIT_ENJOY_VB"(some errors)
#            "PLURAL_VERB_AFTER_THIS" "HOLDER_COMPOUNDS" "PERS_PRONOUN_AGREEMENT" "IN_WHO"
#            "MCDONALDS" "MISSING_TO_BEFORE_A_VERB"(some errors) "HE_VERB_AGR" "MOST_SOME_OF_NNS"
#            "ADVISE_VBG" "WILL_BASED_ON" "BEEN_PART_AGREEMENT" "BUILT_IN_HYPHEN" "WAY_COMPOUNDS"
#            "DT_DT" "NON3PRS_VERB" "THE_THEY" "DEPEND_ON" "MOST_OF_THE_TIMES" "IF_VB_PCT"
#            "ALL_NN" "MICROSOFT_PRODUCTS" "HE_D_VBD" "THERE_VBP_NN" "EVERY_EACH_SINGULAR"
#            "THE_SAME_AS" "THIS_MISSING_VERB" "MAN_COMPOUNDS" "FEWER_LESS" "A_GOOGLE"
#            "PRP_HAVE_VB" "BUNCH_OF" "PLACE_COMPOUNDS" "AFTERMARKET" "MD_BE_NON_VBP"
#            "A_INFORMATION" "PRP_VBG" "THE_WORSE_OF" "EVER_NN" "PRINT_COMPOUNDS"
#            "ACCORDING_TO_ME" "KIND_OF_A" "HOUSE_COMPOUNDS" "COMP_THAN" "WOLD_WOULD"
#            "ALL_MOST_SOME_OF_NOUN" "AM_I" "SOME_FACULTY" "YOUR_RE" "THEIR_S" "U_RE"
#            "MANY_NN" "AGREEMENT_SENT_START"  "AUXILIARY_DO_WITH_INCORRECT_VERB_FORM"
#            "LOTS_OF_NN" "THIS_NNS" "NON_ACTION_CONTINUOUS" "DOUBLE_NEGATIVE" "COPY_PASTE"
#            "AFFORD_VBG" "AFFORD_VBG" "THERE_RE_MANY" "A_RB_NN" "RELATIVE_CLAUSE_AGREEMENT"
#             "TO_NON_BASE" "YOU_TUBE" "SOFTWARES" "NEITHER_NOR"  "MUCH_COUNTABLE" "OVER_COMPOUNDS"
            # "LINE_COMPOUNDS" "CONDITIONAL_CLAUSE" "PRP_RB_JJ" "DOES_X_HAS" "AND_END" "RELY_ON" "A_UNCOUNTABLE"
            # "THERE_MISSING_VERB" "BREAKER_COMPOUNDS" "SUPPOSE_TO" "A_BIT" "CD_NN" "LIFE_COMPOUNDS" "MORE_A_JJ"
            # "SINGULAR_VERB_AFTER_THESE_OR_THOSE" "EXPLAIN_TO" "A_LOT_OF_NN" "MANY_NN_U" "THERE_WAS_MANY"
            # "SINGULAR_AGREEMENT_SENT_START" "IN_THE_MEAN_TIME_PHRASE" "IN_TERM_OF_PHRASE" "SIGN_INTO"
            # "PRP_PAST_PART" "ALLOW_TO_DO" "COUNTER_COMPOUNDS" "DID_BASEFORM" "PROGRESSIVE_VERBS" "PEOPLE_VBZ"
            # "THE_MOST"  "HOMO_SAPIENS" "TO_RB_TO_VB" "UP_COMPOUNDS"  "PRP_HAVES"  "BOTH_AS_WELL_AS"
            # "DID_FOUND_AMBIGUOUS" "DOES_NP_VBZ" "WHAT_IS_YOU"  "SUPERIOR_THAN_TO" "THE_ARE"  "PRP_DON"
            # "BASE_FORM" "ONE_PLURAL" "ON_COMPOUNDS" "HAPPY_EASTER" "DID_PAST" "MISSING_ARTICLE" "NOTE_COMPOUNDS"
            # "SOME_NN_VBP" "GAMEBOY" "HE_NEED" "I_MA" "IN_WEEKDAY" "BE_VBP_IN"   "AFFORD_VB" "IF_THERE"
            # "AGREEMENT_SENT_START_2" "EQUALLY_AS"   "PRP_FOND" "BACK_COMPOUNDS" "THE_BEST_WAY"  "CROSS_COMPOUNDS"
            # "OUT_COMPOUNDS" "PRP_RB_NO_VB" "BE_IS" "HAD_VBP" "ROOM_COMPOUNDS" "ALLOW_TO" "BE_NO_VB" "BE_NO_VB"
            # "A_BIT_OF" "SPLITTED"  "ON_FIRST_GLANCE" "AN_ANOTHER" "SUPERLATIVE_THAN" "CAUSE_BECAUSE "DIE_DICE"
            # "EYE_COMPOUNDS" "HOW_DO_I_VB" "STRIKED" "THANK_FULL" "TWO_CONNECTED_MODAL_VERBS" "IS_VBZ" "THERE_AFTER"
            # "MATE_COMPOUNDS" "BE_TO_VBG" "MANY_TIME" "PROOF_COMPOUNDS" "FURTHER_MORE_COMPOUND" "HARDWARES"
            # "THESE_ONES" "IT_ITS" "AS_ADJ_AS" "DOESENT" "TAKEAWAY" "SHORT_COMPARATIVES"  "REGARDING_TO"
            # "HEAD_COMPOUNDS" "DO_HE_VERB" "DETERMINER_GEOGRAPHICAL_WORD"  "ABLE_VBP" "WORK_AROUND_COMPOUND"
            # "DOWN_COMPOUNDS"  "WOULD_BE_JJ_VB" "POSSESSIVE_CASE" "WHAT_TO_VBD"  "TOO_EITHER" "DT_RB_IN" 
            # "THIS_NNS_VB"  "SHIP_COMPOUNDS" "PRP_NO_VB" THE_EXACTLY_THE "BELIVE_BELIEVE" "MOST_SUPERLATIVE"
            # "THIS_TOOLS" "I_NOT_JJ" "DO_VBZ" "BE_INTEREST_IN", "AN_VB_PRP" (with mistakes)


grammar_spelling_analysis <- subset(grammar_spelling_analysis, 
                                    !(ruleId %in% c("MISSING_TO_BETWEEN_BE_AND_VB", 
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
                                                    "MAKE_UP","USE_TO_VERB", "SUBJECT_MATTER_HYPHEN", 
                                                    "LOGGED_IN_HYPHEN", "CLICK_HYPHEN",  "GOOGLE_PRODUCTS", 
                                                    "PRP_MD_NN",  "THE_PUNCT",  "COLLECTIVE_NOUN_VERB_AGREEMENT_VBP",
                                                    "APPLE_PRODUCTS", "WORD_ESSAY_HYPHEN", 
                                                    "OBJECTIVE_CASE", "RUNNER_UP_HYPHEN", 
                                                    "MUST_HAVE","PRP_THE", "TOO_ADJECTIVE_TO",
                                                    "STEP_BY_STEP_HYPHEN", 
                                                    "THE_IT",  "DT_JJ_NO_NOUN", "GO_TO_HYPHEN",
                                                    "POKEMON", "NO_GO_HYPHEN", "A_THANK_YOU", 
                                                    "WEEK_LONG_HYPHEN", 
                                                    "OPT_IN_HYPHEN",  "CALL_TO_ACTION_HYPHEN", 
                                                    "MARKDOWN_NNP", "CA_BRAND_NEW", "HAVE_TWITTER", 
                                                    "TOP_DOWN_HYPHEN", 
                                                    "OPEN_OFFICE", "SET_TOP_BOX_HYPHEN", 
                                                    "ADVERB_OR_HYPHENATED_ADJECTIVE",
                                                    "MONEY_BACK_HYPHEN", "THE_HOT_DOG", 
                                                    "SOON_TO_BE_HYPHEN", "HIGH_END_HYPHEN", 
                                                    "THOUGH_THOUGHT",  "CLEAR_CUT_HYPHEN", 
                                                    "THE_NN_AND_THE_NN", 
                                                    "ARTICLE_ADJECTIVE_OF", "MUST_BE_DO", 
                                                    "MISSING_NOUN", "YEAR_HYPHEN", "MISSING_PAST_TENSE", 
                                                    "SO_CALLED_HYPHEN", 
                                                    "AN_ARE", "A_COLLECTIVE_OF_NN", 
                                                    "NEEDNT_TO_DO_AND_DONT_NEED_DO", 
                                                    "CATCH_ALL_HYPHEN", "MUCH_NEEDED_HYPHEN", 
                                                    "DAY_TO_DAY_HYPHEN", "PROBLEM_SOLVE_HYPHEN",
                                                    "HAD_TOO", "NON_ANTI_JJ", "MENTION_ABOUT", "MISSING_PREPOSITION",
                                                    "ADVERTISEMENT_OF_FOR"
                                                    )))
                                                    
                                                    
                                                    
bar = subset(data_str_tr, Id == 29994)
bar$body
bar$WithoutHTML
substr(bar$WithoutHTML, 30, 90)
# subset(incorrect_dict, Id == 24673)

# we don't consider errors the following 
grammar_spelling_analysis <- subset(grammar_spelling_analysis, 
                                    !(category %in% c("REDUNDANCY", "SEMANTICS",
                                                      "BRITISH_ENGLISH", "STYLE")))

grammar_spelling_analysis <- subset(grammar_spelling_analysis, 
                                    !(ruleId %in% c("UPPERCASE_SENTENCE_START", 
                                                    "LINKEDIN", "PAYPAL", 
                                                    "YOUTUBE", "LC_AFTER_PERIOD", 
                                                    "THE_FRENCH", "GITHUB", "WORDPRESS", 
                                                    "ENGLISH_WORD_REPEAT_RULE", 
                                                    "EN_WORD_COHERENCY", "RED_READ", 
                                                    "NO_KNOW", "LOG_IN", 
                                                    "TO_TWO", "EVERYDAY_EVERY_DAY", 
                                                    "IS_US", "VBG_THEYRE", 
                                                    "LOOK_WATCH", "WERE_WHERE", 
                                                    "CONFUSION_DUE_DO", 
                                                    "PERSONAL_PERSONNEL", "BY_BUY", 
                                                    "ECONOMICAL_ECONOMIC", 
                                                    "A_INFINITIVE", # grammar
                                                    "DT_PRP", # due to deleting html formatting 
                                                    "SENTENCE_FRAGMENT",
                                                    "PRP_VB", "ADVERB_WORD_ORDER", 
                                                    "SIMPLE_TO_USE_HYPHEN", 
                                                    "PRP_JJ", "FACTOR_HYPHEN", 
                                                    "COLLECTIVE_NOUN_VERB_AGREEMENT_VBD", 
                                                    "DOUBLE_CLICK_HYPHEN", 
                                                    "ANYMORE_ADVERB", "NEWS_COMPOUNDS", 
                                                    "A_WINDOWS")))


# KEEPT: 
# TYPOGRAPHY: "WRONG_APOSTROPHE", "SPURIOUS_APOSTROPHE"
# PUNCTUATION: "SENT_START_CONJUNCTIVE_LINKING_ADVERB_COMMA" "QUESTION_MARK" "NO_COMMA_BEFORE_INDIRECT_QUESTION"
#               "E_G" "I_E" "EG_SPACE" "APOS_ARE"
# TYPOS: "MORFOLOGIK_RULE_EN_US" (accept British English, compare with the one found in the incorrect dict), 
#        "I_LOWERCASE" "IS_SHOULD" "IT_IS" (there are some mistakes though) "OUT_COME" "MISSING_GENITIVE"
#        "POSSESSIVE_APOSTROPHE" "LAYS_ATOP" "IM_AM" "EN_CONTRACTION_SPELLING" "LETS_LET" "SETUP_VERB"
#        "NON_STANDARD_WORD" (check with incorrect dictionary) "ABOUT_ITS_NN" "ANY_MORE" "COMPARISONS_THEN"
#        "AFTERALL" "IT_IS_2" "EN_DIACRITICS_REPLACE" "LIGHT_WEIGHT" "CONFUSION_OF_THEN_THAN" "CANT" "FOR_AWHILE"
#        "WHERE_AS" "YOUR_SHOULD" "TEL_TELL" "THE_SOME_DAY" "THROUGH_OUT" "ANY_BODY" "META_DATA""ALLOT_OF"
#        "TO_VB_ITS_NN" "LOOSING_EFFORTRECORDSEASON" "TO_COMEBACK" "YOUR_YOU" "WORTH_WHILE" "BARE_IN_MIND" "WIFI"
#        "IF_OF" "NOW_A_DAYS" "INCORRECT_CONTRACTIONS" "WORLD_WIDE" "AM_LOATHE_TO" "CA_PRP" "FROM_FORM" "WHOS_NN"
#        "THEM_SELVES" "MI" "VERB_APOSTROPHE_S" "THEE" "THINK_OFF" "WHOM_WHO" "ITS_IS" "WITH_OUT" "ER" "CAN_BACKUP"
#        "THERE_FORE" "ET_AL" "ONE_OF_THE_ONLY" "TIS" "HASNT_IRREGULAR_VERB" "HE_THE" "ALONG_SIDE" "ANINFOR_EVERY_DAY"
#        "NEAR_BY" "KEY_WORDS" "OUT_SIDE" "PERS_PRON_CONTRACTION" "SENT_START_THEM"  "STAND_ALONE_NN" "ONE_THE_ONE_HAND"
#        "ALONG_TIME" "OM" "FREE_REIGN" "FOR_FRO" "VARY_VERY" "APART_A_PART" "AN_THEN" "ELUDED_TO" "NEED_TO_VBG"
#        "SOME_WHAT_JJ" "BUILD_OFF_OF" "ANY_WHERE" "MODAL_OF" "LOT_S" "INCASE_OF" "HOW_EVER" "ONE_ORE"
# NONSTANDARD_PHRASES: "NONSTANDARD_PHRASES" "INFORMATIONS" "THE_QUESTION_WH"
# STYLE: take out from grammar, but it could be used to profile the users (informal, offensive, etc. )
# COMPOUNDING: "CHECK_BOX_COMPOUND" "SIGN_UP_HYPHEN" "ROLL_OUT_HYPHEN"
# CASING: "EN_SPECIFIC_CASE"
# MISC: "EN_A_VS_AN" "EN_COMPOUNDS"
# "CONFUSED_WORDS": "LESS_MORE_THEN" "DOSE_DOES" "TO_TOO" (some mistakes) "INSURE_THAT" "COMMA_THAN" (some mistakes)
#                   "KNOW_NOW" (some mistakes) "MAY_MANY" "THERE_THEIR" "DO_TO" "THERE_OWN"  "YOUR_NN" "LOSE_LOSS"
#                   "TOO_TO" "NO_NOT" "THE_LATER_LATTER" "YOUR" "SOMETIME_SOMETIMES" "ASK_WETHER" "THEY_WHERE"
#                   "AND_THAN" "TAT" "ARCHITECT_VERB" "AFFECT_EFFECT" "YO_TO" "CONFUSION_OF_MANS_MEN" "CLEAN_UP"
#                   "RATHER_THEN" "ADJECTIVE_ADVERB" "THE_THEM" "THEIR_IS" "CALENDER" "AN_AND" "INTENT_INTEND"
#                   "COULD_OF" "ASSES_ASSESS" "DELIVERY_DELIVER" "THING_THINK" "IN_PRINCIPAL" "THAT_THAN"
#                   "WORK_AS_A_CHARM" "DEPENDENT" "WHAT_IT_HAPPENING" "FORE_FOR" "TRAIL_TRIAL" "PRECEDENT_PRECEDENCE"
#                   "BACK_IN_FORTH" "TROUGH_THROUGH" "IT_SEAMS" "GIVE_ADVISE" "ANS_AND" "CONFUSION_OF_SATE_STATE"
#                   "QUIET_QUITE" "BESIDES_BESIDE" "WAN_WANT" "WHAT_IT_THE" "ADVICE_ADVISE" "BLU_RAY" "MAY_MANY_MY"


# Convenience function to convert html codes
html2txt <- function(str) {
xpathApply(htmlParse(str, asText=TRUE),
                "//body//text()", 
                xmlValue)
}



# Initialize the dataframe and the row count
original_text <- data.frame(Id = as.numeric(),
                             content = as.character(),
                             stringsAsFactors=FALSE)
row = 1


for (i in seq(nrow(data_str_tr))) {
  if (!is.na(data_str_tr$body[i])) {
    
    text <- html2txt(data_str_tr$body[i])
      
    for (n in seq(length(text))) {
      original_text[row, 1] <- data_str_tr$Id[i]
      original_text[row, 2] <- text[n]
      row = row + 1
    }
  }
}

write.csv(original_text,
          "C:/Projects/Stack_Exchange/01_motivation_feedback/Answers/data/raw/original_text.csv",
          fileEncoding ="UTF-8",
          row.names = FALSE)



# run languagetool again and check the difference

# TODO <code>
foo <- subset(data_str_tr, Id=="28698")




# TODO check PostHistory for deleted posts

# REPUTATION
# TODO https://stackexchange.com/users/7779041/%E6%96%AD%E6%A1%A5%E5%A6%B9%E5%A6%B9?tab=reputation&sort=post&page=1
# https://ux.stackexchange.com/users/79167/%e6%96%ad%e6%a1%a5%e5%a6%b9%e5%a6%b9?tab=reputation
setwd("C:/Projects/Stack_Exchange/01_motivation_feedback/Answers/data/raw")
reputation <- read.csv("Reputation.csv", stringsAsFactors = FALSE) 

# time formatting
reputation$time_UTC <- as.POSIXct(substr(reputation$time_UTC,1,nchar(reputation$time_UTC)-1), 
                                  format = "%Y-%m-%d %H:%M:%S", tz = "UTC")

# Calculate totRep until CreationTime
reputation_df <- merge(data_str_tr[,c("OwnerUserId", "Id", "CreationDate")], reputation, 
                       by = "OwnerUserId", all.x = TRUE)


reputation_sum <- reputation_df %>%
  group_by(Id) %>%
  summarize(TotRep = sum(reputation[time_UTC <= CreationDate]))

# add 1 reputation points to all. 
# 1 point is given as default when you register 
reputation_sum$TotRep <- ifelse(is.na(reputation_sum$TotRep), 0, reputation_sum$TotRep)
reputation_sum$TotRep <- reputation_sum$TotRep + 1

# Merge
data_str_tr <- merge(data_str_tr, reputation_sum, 
                     by = "Id", all.x = TRUE)
rm(reputation, reputation_df, reputation_sum)
