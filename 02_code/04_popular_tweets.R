#*****************************************************************************# 
# Purpose: Create tables of most popular tweets                               #
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
tweet<-read.csv("Data/latinos.csv", encoding = "UTF-8")

tweet$year <- substr(tweet$created_at, 1, 4)
tweet$lang <- substr(tweet$File, 4, 5)
tweet$File <- paste(tweet$lang, tweet$year, sep = "")

demog<-read.csv("Data/demog2019_2021_pt_sp.csv", encoding = "UTF-8")

# *****************************************************************************
#### 04_Tweets with highest number of RT ####
# *****************************************************************************

tweet_score_19 <- tweet %>%
  filter(File=="es2019") %>%
  mutate(verified = case_when(user_verified == "True" ~ "Verified",
                              user_verified == "False" ~ "Not verified", 
                              user_verified == ""  ~ "Unknown")) %>%
  select(user_id, username, verified, tweet, replies_count, retweets_count, likes_count) 

tweet_score_21 <- tweet %>%
  filter(File=="es2021")  %>%
  mutate(verified = case_when(user_verified == "True" ~ "Verified",
                              user_verified == "False" ~ "Not verified", 
                              user_verified == ""  ~ "Unknown")) %>%
  select(user_id, username, verified, tweet, replies_count, retweets_count, likes_count) 


# 2019

# Sort db based on retweets
tweet_rt_19 <- tweet_score_19 %>%
  arrange(-retweets_count) %>%
  select(username, verified, tweet, retweets_count) %>%
  mutate(Retweets = as.character(retweets_count)) %>%
  slice(1:15) %>%
  select(username, verified, tweet, Retweets)

dust(tweet_rt_19) %>%
  kable(align = "lll",
        format = "html") %>%
  kable_styling() %>%
  save_kable("tweet_rt_sp_19.html")

# Sort db based on replies
tweet_rp_19 <- tweet_score_19 %>%
  arrange(-replies_count) %>%
  select(username, verified, tweet, replies_count) %>%
  mutate(Replies = as.character(replies_count)) %>%
  slice(1:15) %>%
  select(username, verified, tweet, Replies)

dust(tweet_rp_19) %>%
  kable(align = "lll",
        format = "html") %>%
  kable_styling() %>%
  save_kable("tweet_rp_sp_19.html")


# Sort db based on likes
tweet_lk_19 <- tweet_score_19 %>%
  arrange(-likes_count) %>%
  select(username, verified, tweet, likes_count) %>%
  mutate(Likes = as.character(likes_count)) %>%
  slice(1:15) %>%
  select(username, verified, tweet, Likes)

dust(tweet_lk_19) %>%
  kable(align = "lll",
        format = "html") %>%
  kable_styling() %>%
  save_kable("tweet_lk_sp_19.html")

# 2021

# Sort db based on retweets
tweet_rt_21 <- tweet_score_21 %>%
  arrange(-retweets_count) %>%
  select(username, verified, tweet, retweets_count) %>%
  mutate(Retweets = as.character(retweets_count)) %>%
  slice(1:15) %>%
  select(username, verified, tweet, Retweets)

dust(tweet_rt_21) %>%
  kable(align = "lll",
        format = "html") %>%
  kable_styling() %>%
  save_kable("tweet_rt_sp_21.html")

# Sort db based on replies
tweet_rp_21 <- tweet_score_21 %>%
  arrange(-replies_count) %>%
  select(username, verified, tweet, replies_count) %>%
  mutate(Replies = as.character(replies_count)) %>%
  slice(1:15) %>%
  select(username, verified, tweet, Replies)

dust(tweet_rp_21) %>%
  kable(align = "lll",
        format = "html") %>%
  kable_styling() %>%
  save_kable("tweet_rp_sp_21.html")


# Sort db based on likes
tweet_lk_21 <- tweet_score_21 %>%
  arrange(-likes_count) %>%
  select(username, verified, tweet, likes_count) %>%
  mutate(Likes = as.character(likes_count)) %>%
  slice(1:15) %>%
  select(username, verified, tweet, Likes)

dust(tweet_lk_21) %>%
  kable(align = "lll",
        format = "html") %>%
  kable_styling() %>%
  save_kable("tweet_lk_sp_21.html")

