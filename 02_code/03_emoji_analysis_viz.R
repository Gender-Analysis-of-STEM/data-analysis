#*****************************************************************************# 
# Purpose: Plot Emoji Analysis of the F2BD project                            #
#                                                                             #
# Created: September 21, 2021                                                 #
# Depends on:                                                                 #
#   Author: Manuel Cardona                                                    # 
#   E-mail: mcardona@poverty-action.org                                       #
#                                                                             #
#*****************************************************************************# 

rm(list = ls()) # to clean the workspace

# *****************************************************************************
#### 01_Load_packages ####
# *****************************************************************************

library(tidyverse)
library(lubridate)
library(ggplot2)
library(dplyr)
library(readxl)
library(haven)
library(devtools)
library(dash)
library(dampack)
library(dygraphs)
library(xts)
library(paletteer) 
library(circlize)
library(pixiedust)
library(kableExtra)
library(igraph)
library(ore)
library(stringi)
library(rtweet)
library(rvest)
library(Unicode)
library(tm)
library(png)
library(grid)


# *****************************************************************************
#### 02_Set directories ####
# *****************************************************************************
setwd("../../../../Dropbox/F2BD Literature Review/")

# *****************************************************************************
#### 03_Load_data ####
# *****************************************************************************
tweet<-read.csv("Data/database_sp_cleaned_topics.csv", encoding = "UTF-8")

# *****************************************************************************
#### 04_Extract emojis from tweets ####
# *****************************************************************************
Sys.setlocale(category = "LC_ALL", locale = "en_US.UTF-8")

## ---- utility functions ----
# this function outputs the emojis found in a string as well as their occurences
count_matches <- function(string, matchto, description, sentiment = NA) {
  
  vec <- str_count(string, matchto)
  matches <- which(vec != 0)
  
  descr <- NA
  cnt <- NA
  
  if (length(matches) != 0) {
    
    descr <- description[matches]
    cnt <- vec[matches]
    
  } 
  
  df <- data.frame(text = string, description = descr, count = cnt, sentiment = NA)
  
  if (!is.na(sentiment) && length(sentiment[matches]) != 0) {
    
    df$sentiment <- sentiment[matches]
    
  }
  
  return(df)
  
}

# this function applies count_matches on a vector of texts and outputs a data.frame
emojis_matching <- function(texts, matchto, description, sentiment = NA) {
  
  texts %>% 
    map_df(count_matches, 
           matchto = matchto, 
           description = description, 
           sentiment = sentiment)
  
}

# function that separates capital letters hashtags
hashgrep <- function(text) {
  hg <- function(text) {
    result <- ""
    while(text != result) {
      result <- text
      text <- gsub("#[[:alpha:]]+\\K([[:upper:]]+)", " \\1", text, perl = TRUE)
    }
    return(text)
  }
  unname(sapply(text, hg))
}

# tweets cleaning pipe
cleanPosts <- function(text) {
  clean_texts <- text %>%
    gsub("<.*>", "", .) %>% # remove emojis
    gsub("&amp;", "", .) %>% # remove &
    gsub("(RT|via)((?:\\b\\W*@\\w+)+)", "", .) %>% # remove retweet entities
    gsub("@\\w+", "", .) %>% # remove at people
    hashgrep %>%
    gsub("[[:punct:]]", "", .) %>% # remove punctuation
    gsub("[[:digit:]]", "", .) %>% # remove digits
    gsub("http\\w+", "", .) %>% # remove html links
    iconv(from = "latin1", to = "ASCII", sub="") %>% # remove emoji and bizarre signs
    gsub("[ \t]{2,}", " ", .) %>% # remove unnecessary spaces
    gsub("^\\s+|\\s+$", "", .) %>% # remove unnecessary spaces
    tolower
  return(clean_texts)
}

# function that outputs a df of emojis with their top 5 words (by frequency)
wordFreqEmojis <- function(df, text = df$text, description = df$description, top = 5) {
  
  map_df(unique(description), function(x) {
    
    dat <- df %>% 
      filter(description == x)
    
    myCorpus <- Corpus(VectorSource(dat$text)) %>%
      tm_map(removePunctuation) %>%
      tm_map(stripWhitespace) %>%
      tm_map(removeWords, stopwords("english"))
    
    dtm <- DocumentTermMatrix(myCorpus)
    # find the sum of words in each Document
    rowTotals <- apply(dtm , 1, sum)
    dtm.new   <- dtm[rowTotals> 0, ]
    # collapse matrix by summing over columns
    freq <- colSums(as.matrix(dtm))
    # create sort order (descending)
    ord <- order(freq, decreasing = TRUE)
    
    list(emoji = rep(x, top), 
         words = names(freq[ord][1:top]), 
         frequency = freq[ord][1:top]) 
    
  })
  
}



## ---- setup ----
# read in emoji dictionary
# https://raw.githubusercontent.com/today-is-a-good-day/emojis/master/emojis.csv
# input your custom path to file

emojis <- read.delim("C:/Users/Manuel Cardona Arias/Dropbox/F2BD Literature Review/Data/emojis.txt", encoding="UTF-8")
  #This file contains the encoding and description of all existent emojis in both English and Spanish

emDict_raw <- emojis %>% 
  select(description = EN, r_encoding = ftu8, unicode)

# plain skin tones
skin_tones <- c("light skin tone", 
                "medium-light skin tone", 
                "medium skin tone",
                "medium-dark skin tone", 
                "dark skin tone")

# remove plain skin tones and remove skin tone info in description
emDict <- emDict_raw %>%
  # remove plain skin tones emojis
  filter(!description %in% skin_tones) %>%
  # remove emojis with skin tones info, e.g. remove woman: light skin tone and only
  # keep woman
  filter(!grepl(":", description)) %>%
  mutate(description = tolower(description)) %>%
  mutate(unicode = as.u_char(unicode))
# all emojis with more than one unicode codepoint become NA 

matchto <- emDict$r_encoding
description <- emDict$description

# get text data
raw_usermedia <- tweet
# convert to a format we can work with
usermedia <- raw_usermedia %>% 
  mutate(text = iconv(tweet, from = "latin1", to = "ascii", sub = "byte"))

## ---- most used emoji ----
# rank emojis by occurence in data
options(stringsAsFactors = FALSE)

rank <- emojis_matching(usermedia$text, matchto, description) %>% 
  group_by(description) %>% 
  summarise(n = sum(count, na.rm = TRUE)) %>%
  arrange(-n)

# *****************************************************************************
#### 05_Occurrence of emojis ####
# *****************************************************************************

head(rank, 10)

rank$rank <- seq(1,dim(rank)[1],1)
rank_top10 <- subset(rank, rank <= 10)

xlab <- 'Rank' 
ylab <- 'Overall Occurrence'

setwd("../F2BD Literature Review/Figures/_emoji/")
imgs <- lapply(paste0(rank_top10$description, '.png'), png::readPNG)
g <- lapply(imgs, grid::rasterGrob)

k <- 0.20 * (10/nrow(rank_top10)) * max(rank_top10$n)
rank_top10$xsize <- k
rank_top10$ysize <- k
rank_top10$ysize <- k * (rank_top10$n / max(rank_top10$n))
rank_top10$ysize <- 149


g1 <- ggplot(data = rank_top10, aes(x = rank, y = n)) +
  geom_bar(stat = 'identity', fill = 'dodgerblue4') +
  xlab(xlab) + ylab(ylab) +
  mapply(function(x, y, i) {
    annotation_custom(g[[i]], xmin = x-0.5*rank_top10$xsize[i], xmax = x+0.5*rank_top10$xsize[i], 
                      ymin = y-0.5*rank_top10$ysize[i], ymax = y+0.5*rank_top10$ysize[i])},
    rank_top10$rank, rank_top10$n, seq_len(nrow(rank_top10))) +
  scale_x_continuous(expand = c(0, 0), breaks = seq(1, nrow(rank_top10), 1), labels = seq(1, nrow(rank_top10), 1)) +
  scale_y_continuous(expand = c(0, 0), limits = c(0, 1.10 * max(rank_top10$n)))+
  theme_minimal() +
  theme(plot.title = element_text(face = "bold",
                                  size = 20,
                                  family = "Arial"),
        plot.subtitle = element_text(size = 12,
                                     face = "plain", 
                                     family = "Arial"),
        plot.caption = element_text(hjust = 0, 
                                    face = "plain", 
                                    family = "Arial",
                                    size = 8,
                                    colour = "#777777"),
        panel.background = element_rect(fill = "white", 
                                        colour = "white", 
                                        size = 0.15, 
                                        linetype = "solid"),
        panel.grid.major = element_line(size = 0.15, 
                                        linetype = 'solid',
                                        colour = "gray50"), 
        panel.grid.minor = element_line(size = 0.15, 
                                        linetype = 'solid',
                                        colour = "gray50"), 
        axis.title.x = (element_text(size = 16,
                                     family = "Arial")),
        axis.title.y = (element_text(size =16,
                                     family = "Arial")),
        element_line(linetype = "dotted",
                     colour = "gray99",
                     size = .1),
        axis.text.x = element_text(angle = 0,
                                   hjust = 0.5,
                                   size = 12, 
                                   family = ""),
        axis.text.y = element_text(size = 9,
                                   family = ""),
        legend.text = element_text(size = 12,
                                   family = ""),
        legend.title = element_blank()) +
  labs(title = "Occurrence of emojis",
       x = "Rank",
       y = "Overall Occurrence",
       caption = paste(" ")) +
  ggsave(paste0("01", "_", "emoji_occurrence", ".jpeg"), 
         width = 10, height = 6)


# *****************************************************************************
#### 06_Tweets with highest number of RT ####
# *****************************************************************************

tweet_score <- tweet %>%
  select(user_id, username, tweet, replies_count, retweets_count, likes_count)

# Sort db based on retweets
tweet_rt <- tweet_score %>%
  arrange(-retweets_count) %>%
  select(username, tweet, retweets_count) %>%
  mutate(Retweets = as.character(retweets_count)) %>%
  slice(1:15) %>%
  select(username, tweet, Retweets)

dust(tweet_rt) %>%
  kable(align = "lll",
        format = "html") %>%
  kable_styling() %>%
  save_kable("tweet_rt.html")

# Sort db based on replies
tweet_rp <- tweet_score %>%
  arrange(-replies_count) %>%
  select(username, tweet, replies_count) %>%
  mutate(Replies = as.character(replies_count)) %>%
  slice(1:15) %>%
  select(username, tweet, Replies)

dust(tweet_rp) %>%
  kable(align = "lll",
        format = "html") %>%
  kable_styling() %>%
  save_kable("tweet_rp.html")


# Sort db based on likes
tweet_lk <- tweet_score %>%
  arrange(-likes_count) %>%
  select(username, tweet, likes_count) %>%
  mutate(Likes = as.character(likes_count)) %>%
  slice(1:15) %>%
  select(username, tweet, Likes)

dust(tweet_lk) %>%
  kable(align = "lll",
        format = "html") %>%
  kable_styling() %>%
  save_kable("tweet_lk.html")






## ---- tweets with most emojis ----
tweets <- emojis_matching(usermedia$text, matchto, description) %>% 
  group_by(text) %>% 
  summarise(n = sum(count, na.rm = TRUE)) %>%
  # I add the time created because it makes it easiert to look up certain tweets
  merge(usermedia, by = "text") %>% 
  select(text, n, created_at) %>%
  arrange(-n)

mean(tweets$n, na.rm = TRUE)


## ---- sentiment analysis with emojis ---- 
# reference website
url <- "http://kt.ijs.si/data/Emoji_sentiment_ranking/index.html"

# get emoticons
emojis_raw <- url %>%
  read_html() %>%
  html_table() %>%
  data.frame() %>%
  select(-Image.twemoji., -Sentiment.bar.c.i..95..)
names(emojis_raw) <- c("char", "unicode", "occurrences", "position", "negative", 
                       "neutral", "positive", "sentiment_score", "description", 
                       "block")

# change numeric unicode to character unicode to be able to match with emDict 
emojis <- emojis_raw %>%
  mutate(unicode = as.u_char(unicode)) %>%
  mutate(description = tolower(description)) 

str(emojis)
# unicode column is unicode character class

# merge with emDict to get encoding
emojis_merged <- emojis %>%
  merge(emDict, by = "unicode")
# emojis %>% filter(!unicode %in% emDict$unicode) %>% View
# we loose 137 emojis that are not in emDict and for which we don't have an R encoding
# but they seem to be black and white emojis not too often used in social media anyways

new_matchto <- emojis_merged$r_encoding
new_description <- emojis_merged$description.x
sentiment <- emojis_merged$sentiment_score

sentiments <- emojis_matching(usermedia$text, new_matchto, new_description, sentiment) %>%
  mutate(sentiment = count * as.numeric(sentiment)) %>%
  group_by(text) %>% 
  summarise(sentiment_score = sum(sentiment, na.rm = TRUE))

usermedia_merged <- usermedia %>% 
  select(text, created_at) %>% 
  merge(sentiments, by = "text", all.x = TRUE)
# some tweets don't have sentiment scores

# this is how it looksl ike over time:
usermedia_merged %>% 
  mutate(date = as.Date(created_at)) %>% 
  group_by(date) %>% 
  summarise(sent = mean(sentiment_score, na.rm = TRUE)) %>% 
  ggplot + 
  aes(x = date, y = sent) + 
  geom_point() + 
  geom_line()

## ---- emojis associated with words in tweets ----
# tweets
raw_texts <- emojis_matching(usermedia$text, matchto, description) %>% 
  select(-sentiment, -count) %>%
  mutate(text = cleanPosts(text)) %>%
  filter(text != "") %>% 
  filter(!is.na(description))
word_emojis <- wordFreqEmojis(raw_texts, raw_texts$text, raw_texts$description) %>% 
  filter(!is.na(words))

## ---- emojis and weekdays ----
emojis_matching(usermedia$text, matchto, description) %>%
  merge(usermedia %>% select(text, created_at), by = "text") %>% 
  select(description, created_at) %>% 
  mutate(weekday = weekdays(created_at)) %>% 
  select(-created_at) %>% 
  group_by(weekday) %>% 
  summarise(n = n()) %>% 
  arrange(-n)

