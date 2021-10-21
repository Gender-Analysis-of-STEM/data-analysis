#*****************************************************************************# 
# Purpose: Plot Sentiment Analysis of the F2BD project                        #
#                                                                             #
# Created: August 26, 2021                                                    #
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


# *****************************************************************************
#### 02_Set directories ####
# *****************************************************************************
setwd("../Dropbox/F2BD Literature Review/")

# *****************************************************************************
#### 03_Load_data ####
# *****************************************************************************
tweet<-read.csv("Data/sentiment-analysis-pt.csv")

tweet$year <- substr(tweet$created_at, 1, 4)
tweet$lang <- substr(tweet$File, 4, 5)
tweet$File <- paste(tweet$lang, tweet$year, sep = "")


pt_2019 <- tweet %>%
  filter(File=="pt2019")
pt_2021 <- tweet %>%
  filter(File=="pt2021")


palette<-paletteer_d("LaCroixColoR::paired")
palette_2<-paletteer_d("rcartocolor::ag_Sunset")


# *****************************************************************************
#### 04_Sentiment scores ####
# *****************************************************************************

#Sentiments can be classified as positive, neutral or negative. They can also
#be represented on a numeric scale, to better express the degree of positive
#or negative strength of the sentiment contained in a body of text.

#----ORIGINAL TWEETS----

neutral <- length(which(tweet$label == "Neutral"))
positive <- length(which(tweet$label == "Positive"))
negative <- length(which(tweet$label == "Negative"))
Sentiment <- c("Positive","Neutral","Negative")
Count <- c(positive,neutral,negative)
output <- data.frame(Sentiment,Count)
output$Sentiment<-factor(output$Sentiment,levels=Sentiment)

ggplot(output, aes(x=Sentiment,y=Count))+
  geom_bar(alpha=0.7, stat = "identity", aes(fill = Sentiment)) +
  geom_text(aes(label = Count), vjust = 1.5) +
  scale_y_continuous(breaks = number_ticks(20)) +
  theme_minimal() +
  scale_fill_manual(values=c(palette[7], palette[9], palette[11])) +
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
  labs(title = "Sentiment type of tweets",
       subtitle = " ",
       x = " ",
       y = "Tweets",
       caption = paste(" ")) +
  ggsave(paste0("Figures/", "01", "_", "tweet_sentiment_pt", ".jpeg"), 
         width = 10, height = 6)

#----ORIGINAL TWEETS (2019)----

neutral <- length(which(pt_2019$label == "Neutral"))
positive <- length(which(pt_2019$label == "Positive"))
negative <- length(which(pt_2019$label == "Negative"))
Sentiment <- c("Positive","Neutral","Negative")
Count <- c(positive,neutral,negative)
output <- data.frame(Sentiment,Count)
output$Sentiment<-factor(output$Sentiment,levels=Sentiment)

ggplot(output, aes(x=Sentiment,y=Count))+
  geom_bar(alpha=0.7, stat = "identity", aes(fill = Sentiment)) +
  geom_text(aes(label = Count), vjust = 1.5) +
  scale_y_continuous(breaks = number_ticks(20)) +
  theme_minimal() +
  scale_fill_manual(values=c(palette[7], palette[9], palette[11])) +
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
  labs(title = "Sentiment type of tweets (2019)",
       subtitle = " ",
       x = " ",
       y = "Tweets",
       caption = paste(" ")) +
  ggsave(paste0("Figures/", "01", "_", "tweet_sentiment_pt_2019", ".jpeg"), 
         width = 10, height = 6)

#----ORIGINAL TWEETS (2021)----

neutral <- length(which(pt_2021$label == "Neutral"))
positive <- length(which(pt_2021$label == "Positive"))
negative <- length(which(pt_2021$label == "Negative"))
Sentiment <- c("Positive","Neutral","Negative")
Count <- c(positive,neutral,negative)
output <- data.frame(Sentiment,Count)
output$Sentiment<-factor(output$Sentiment,levels=Sentiment)

ggplot(output, aes(x=Sentiment,y=Count))+
  geom_bar(alpha=0.7, stat = "identity", aes(fill = Sentiment)) +
  geom_text(aes(label = Count), vjust = 1.5) +
  scale_y_continuous(breaks = number_ticks(20)) +
  theme_minimal() +
  scale_fill_manual(values=c(palette[7], palette[9], palette[11])) +
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
  labs(title = "Sentiment type of tweets (2021)",
       subtitle = " ",
       x = " ",
       y = "Tweets",
       caption = paste(" ")) +
  ggsave(paste0("Figures/", "01", "_", "tweet_sentiment_pt_2021", ".jpeg"), 
         width = 10, height = 6)

# *****************************************************************************
#### 07_Number of likes, retweets and replies by Senmtiment ####
# *****************************************************************************

#----ORIGINAL TWEETS----

sent <- c("Sentiment",
          "Positive",
          "Negative",
          "Neutral")

tweets <- c("Tweets",
            nrow(tweet[tweet$label=="POS",]), 
            nrow(tweet[tweet$label=="NEG",]), 
            nrow(tweet[tweet$label=="NEU",]))

likes <- c("Likes per tweet",
           round((sum(subset(tweet, label == "POS")$likes_count)/nrow(tweet[tweet$label=="POS",])),2),
           round((sum(subset(tweet, label == "NEG")$likes_count)/nrow(tweet[tweet$label=="NEG",])),2),
           round((sum(subset(tweet, label == "NEU")$likes_count)/nrow(tweet[tweet$label=="NEU",])),2))

replies <- c("Replies per tweet",
             round((sum(subset(tweet, label == "POS")$replies_count)/nrow(tweet[tweet$label=="POS",])),2),
             round((sum(subset(tweet, label == "NEG")$replies_count)/nrow(tweet[tweet$label=="NEG",])),2),
             round((sum(subset(tweet, label == "NEU")$replies_count)/nrow(tweet[tweet$label=="NEU",])),2))

retweets <- c("Retweets per tweet",
              round((sum(subset(tweet, label == "POS")$retweets_count)/nrow(tweet[tweet$label=="POS",])),2),
              round((sum(subset(tweet, label == "NEG")$retweets_count)/nrow(tweet[tweet$label=="NEG",])),2),
              round((sum(subset(tweet, label == "NEU")$retweets_count)/nrow(tweet[tweet$label=="NEU",])),2))

table_1 <- data.frame(sent, tweets, likes, replies, retweets)
table_1 <- table_1[-1,]
table_1 <- rename(table_1, "Sentiment" = sent)
table_1 <- rename(table_1, "Number of tweets" = tweets)
table_1 <- rename(table_1, "Likes per tweet" = likes)
table_1 <- rename(table_1, "Replies per tweet" = replies)
table_1 <- rename(table_1, "Retweets per tweet" = retweets)

dust(table_1) %>%
  kable(align = "lllll",
        format = "html") %>%
  kable_styling() %>%
  save_kable("Figures/sum_stats_tweets_pt.html")

#----ORIGINAL TWEETS (2019)----

sent <- c("Sentiment",
          "Positive",
          "Negative",
          "Neutral")

tweets <- c("Tweets",
            nrow(pt_2019[pt_2019$label=="POS",]), 
            nrow(pt_2019[pt_2019$label=="NEG",]), 
            nrow(pt_2019[pt_2019$label=="NEU",]))

likes <- c("Likes per tweet",
           round((sum(subset(pt_2019, label == "POS")$likes_count)/nrow(pt_2019[pt_2019$label=="POS",])),2),
           round((sum(subset(pt_2019, label == "NEG")$likes_count)/nrow(pt_2019[pt_2019$label=="NEG",])),2),
           round((sum(subset(pt_2019, label == "NEU")$likes_count)/nrow(pt_2019[pt_2019$label=="NEU",])),2))

replies <- c("Replies per tweet",
             round((sum(subset(pt_2019, label == "POS")$replies_count)/nrow(pt_2019[pt_2019$label=="POS",])),2),
             round((sum(subset(pt_2019, label == "NEG")$replies_count)/nrow(pt_2019[pt_2019$label=="NEG",])),2),
             round((sum(subset(pt_2019, label == "NEU")$replies_count)/nrow(pt_2019[pt_2019$label=="NEU",])),2))

retweets <- c("Retweets per tweet",
              round((sum(subset(pt_2019, label == "POS")$retweets_count)/nrow(pt_2019[pt_2019$label=="POS",])),2),
              round((sum(subset(pt_2019, label == "NEG")$retweets_count)/nrow(pt_2019[pt_2019$label=="NEG",])),2),
              round((sum(subset(pt_2019, label == "NEU")$retweets_count)/nrow(pt_2019[pt_2019$label=="NEU",])),2))

table_1 <- data.frame(sent, tweets, likes, replies, retweets)
table_1 <- table_1[-1,]
table_1 <- rename(table_1, "Sentiment" = sent)
table_1 <- rename(table_1, "Number of tweets" = tweets)
table_1 <- rename(table_1, "Likes per tweet" = likes)
table_1 <- rename(table_1, "Replies per tweet" = replies)
table_1 <- rename(table_1, "Retweets per tweet" = retweets)

dust(table_1) %>%
  kable(align = "lllll",
        format = "html") %>%
  kable_styling() %>%
  save_kable("Figures/sum_stats_tweets_pt_2019.html")

#----ORIGINAL TWEETS (2021)----

sent <- c("Sentiment",
          "Positive",
          "Negative",
          "Neutral")

tweets <- c("Tweets",
            nrow(pt_2021[pt_2021$label=="POS",]), 
            nrow(pt_2021[pt_2021$label=="NEG",]), 
            nrow(pt_2021[pt_2021$label=="NEU",]))

likes <- c("Likes per tweet",
           round((sum(subset(pt_2021, label == "POS")$likes_count)/nrow(pt_2021[pt_2021$label=="POS",])),2),
           round((sum(subset(pt_2021, label == "NEG")$likes_count)/nrow(pt_2021[pt_2021$label=="NEG",])),2),
           round((sum(subset(pt_2021, label == "NEU")$likes_count)/nrow(pt_2021[pt_2021$label=="NEU",])),2))

replies <- c("Replies per tweet",
             round((sum(subset(pt_2021, label == "POS")$replies_count)/nrow(pt_2021[pt_2021$label=="POS",])),2),
             round((sum(subset(pt_2021, label == "NEG")$replies_count)/nrow(pt_2021[pt_2021$label=="NEG",])),2),
             round((sum(subset(pt_2021, label == "NEU")$replies_count)/nrow(pt_2021[pt_2021$label=="NEU",])),2))

retweets <- c("Retweets per tweet",
              round((sum(subset(pt_2021, label == "POS")$retweets_count)/nrow(pt_2021[pt_2021$label=="POS",])),2),
              round((sum(subset(pt_2021, label == "NEG")$retweets_count)/nrow(pt_2021[pt_2021$label=="NEG",])),2),
              round((sum(subset(pt_2021, label == "NEU")$retweets_count)/nrow(pt_2021[pt_2021$label=="NEU",])),2))

table_1 <- data.frame(sent, tweets, likes, replies, retweets)
table_1 <- table_1[-1,]
table_1 <- rename(table_1, "Sentiment" = sent)
table_1 <- rename(table_1, "Number of tweets" = tweets)
table_1 <- rename(table_1, "Likes per tweet" = likes)
table_1 <- rename(table_1, "Replies per tweet" = replies)
table_1 <- rename(table_1, "Retweets per tweet" = retweets)

dust(table_1) %>%
  kable(align = "lllll",
        format = "html") %>%
  kable_styling() %>%
  save_kable("Figures/sum_stats_tweets_pt_2021.html")