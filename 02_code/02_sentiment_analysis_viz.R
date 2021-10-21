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
tweet<-read.csv("Data/sentiment-emotion-analysis-sp.csv")

tweet$year <- substr(tweet$created_at, 1, 4)
tweet$lang <- substr(tweet$File, 4, 5)
tweet$File <- paste(tweet$lang, tweet$year, sep = "")


es_2019 <- tweet %>%
  filter(File=="es2019")
es_2021 <- tweet %>%
  filter(File=="es2021")


palette<-paletteer_d("LaCroixColoR::paired")
palette_2<-paletteer_d("rcartocolor::ag_Sunset")


# *****************************************************************************
#### 04_Sentiment scores ####
# *****************************************************************************

#Sentiments can be classified as positive, neutral or negative. They can also
#be represented on a numeric scale, to better express the degree of positive
#or negative strength of the sentiment contained in a body of text.

#----ORIGINAL TWEETS----

neutral <- length(which(tweet$output == "NEU"))
positive <- length(which(tweet$output == "POS"))
negative <- length(which(tweet$output == "NEG"))
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
  ggsave(paste0("Figures/", "01", "_", "tweet_sentiment_es", ".jpeg"), 
         width = 10, height = 6)

#----ORIGINAL TWEETS (2019)----

neutral <- length(which(es_2019$output == "NEU"))
positive <- length(which(es_2019$output == "POS"))
negative <- length(which(es_2019$output == "NEG"))
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
  ggsave(paste0("Figures/", "01", "_", "tweet_sentiment_es_2019", ".jpeg"), 
         width = 10, height = 6)

#----ORIGINAL TWEETS (2021)----

neutral <- length(which(es_2021$output == "NEU"))
positive <- length(which(es_2021$output == "POS"))
negative <- length(which(es_2021$output == "NEG"))
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
  ggsave(paste0("Figures/", "01", "_", "tweet_sentiment_es_2021", ".jpeg"), 
         width = 10, height = 6)

#----Tweets from conversations----

neutral <- length(which(thread$output == "NEU"))
positive <- length(which(thread$output == "POS"))
negative <- length(which(thread$output == "NEG"))
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
  labs(title = "Sentiment type of tweets/replies",
       subtitle = " ",
       x = " ",
       y = "Tweets/replies",
       caption = paste(" ")) +
  ggsave(paste0("Figures/", "01", "_", "thread_sentiment", ".jpeg"), 
         width = 10, height = 6)

# *****************************************************************************


#### 05_Emotion classification ####
# *****************************************************************************

#Emotion classificationn is built on a Lexicon, which is a list of words and
#their associations with basic emotions (joj, sadness, anger, surprise, disgust,
#fear, and others).

#----ORIGINAL TWEETS----

anger <- length(which(tweet$output_emotion == "anger"))
disgust <- length(which(tweet$output_emotion == "disgust"))
fear <- length(which(tweet$output_emotion == "fear"))
joy <- length(which(tweet$output_emotion == "joy"))
others <- length(which(tweet$output_emotion == "others"))
sadness <- length(which(tweet$output_emotion == "sadness"))
surprise <- length(which(tweet$output_emotion == "surprise"))

Sentiment <- c("Anger","Disgust","Fear", "Sadness", "Surprise", "Joy", "Others")
Count <- c(anger, disgust, fear, sadness, surprise, joy, others)
output <- data.frame(Sentiment,Count)
output$Sentiment<-factor(output$Sentiment,levels=Sentiment)

ggplot(output, aes(x=Sentiment,y=Count))+
  geom_bar(alpha=0.7, stat = "identity", aes(fill = Sentiment)) +
  geom_text(aes(label = Count), vjust = -0.2) +
  scale_y_continuous(breaks = number_ticks(20)) +
  theme_minimal() +
  scale_fill_manual(values=palette_2) +
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
  labs(title = "Emotion type of tweets",
       subtitle = " ",
       x = " ",
       y = "Tweets",
       caption = paste(" ")) +
  ggsave(paste0("Figures/", "01", "_", "tweet_emotion_es", ".jpeg"), 
         width = 10, height = 6)

#----ORIGINAL TWEETS (2019)----

anger <- length(which(es_2019$output_emotion == "anger"))
disgust <- length(which(es_2019$output_emotion == "disgust"))
fear <- length(which(es_2019$output_emotion == "fear"))
joy <- length(which(es_2019$output_emotion == "joy"))
others <- length(which(es_2019$output_emotion == "others"))
sadness <- length(which(es_2019$output_emotion == "sadness"))
surprise <- length(which(es_2019$output_emotion == "surprise"))

Sentiment <- c("Anger","Disgust","Fear", "Sadness", "Surprise", "Joy", "Others")
Count <- c(anger, disgust, fear, sadness, surprise, joy, others)
output <- data.frame(Sentiment,Count)
output$Sentiment<-factor(output$Sentiment,levels=Sentiment)

ggplot(output, aes(x=Sentiment,y=Count))+
  geom_bar(alpha=0.7, stat = "identity", aes(fill = Sentiment)) +
  geom_text(aes(label = Count), vjust = -0.2) +
  scale_y_continuous(breaks = number_ticks(20)) +
  theme_minimal() +
  scale_fill_manual(values=palette_2) +
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
  labs(title = "Emotion type of tweets (2019)",
       subtitle = " ",
       x = " ",
       y = "Tweets",
       caption = paste(" ")) +
  ggsave(paste0("Figures/", "01", "_", "tweet_emotion_es_2019", ".jpeg"), 
         width = 10, height = 6)

#----ORIGINAL TWEETS (2021)----

anger <- length(which(es_2021$output_emotion == "anger"))
disgust <- length(which(es_2021$output_emotion == "disgust"))
fear <- length(which(es_2021$output_emotion == "fear"))
joy <- length(which(es_2021$output_emotion == "joy"))
others <- length(which(es_2021$output_emotion == "others"))
sadness <- length(which(es_2021$output_emotion == "sadness"))
surprise <- length(which(es_2021$output_emotion == "surprise"))

Sentiment <- c("Anger","Disgust","Fear", "Sadness", "Surprise", "Joy", "Others")
Count <- c(anger, disgust, fear, sadness, surprise, joy, others)
output <- data.frame(Sentiment,Count)
output$Sentiment<-factor(output$Sentiment,levels=Sentiment)

ggplot(output, aes(x=Sentiment,y=Count))+
  geom_bar(alpha=0.7, stat = "identity", aes(fill = Sentiment)) +
  geom_text(aes(label = Count), vjust = -0.2) +
  scale_y_continuous(breaks = number_ticks(20)) +
  theme_minimal() +
  scale_fill_manual(values=palette_2) +
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
  labs(title = "Emotion type of tweets (2021)",
       subtitle = " ",
       x = " ",
       y = "Tweets",
       caption = paste(" ")) +
  ggsave(paste0("Figures/", "01", "_", "tweet_emotion_es_2021", ".jpeg"), 
         width = 10, height = 6)

#----ORIGINAL TWEETS (excluding "others")----

anger <- length(which(tweet$output_emotion == "anger"))
disgust <- length(which(tweet$output_emotion == "disgust"))
fear <- length(which(tweet$output_emotion == "fear"))
joy <- length(which(tweet$output_emotion == "joy"))
#others <- length(which(tweet$output_emotion == "others"))
sadness <- length(which(tweet$output_emotion == "sadness"))
surprise <- length(which(tweet$output_emotion == "surprise"))

Sentiment <- c("Anger","Disgust","Fear", "Sadness", "Surprise", "Joy")
Count <- c(anger, disgust, fear, sadness, surprise, joy)
output <- data.frame(Sentiment,Count)
output$Sentiment<-factor(output$Sentiment,levels=Sentiment)

ggplot(output, aes(x=Sentiment,y=Count))+
  geom_bar(alpha=0.7, stat = "identity", aes(fill = Sentiment)) +
  geom_text(aes(label = Count), vjust = -0.2) +
  scale_y_continuous(breaks = number_ticks(20)) +
  theme_minimal() +
  scale_fill_manual(values=palette_2) +
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
  labs(title = "Emotion type of tweets",
       subtitle = " ",
       x = " ",
       y = "Tweets",
       caption = paste(" ")) +
  ggsave(paste0("Figures/", "01", "_", "tweet_emotion_others_es", ".jpeg"), 
         width = 10, height = 6)

#----ORIGINAL TWEETS (excluding "others") (2019)----

anger <- length(which(es_2019$output_emotion == "anger"))
disgust <- length(which(es_2019$output_emotion == "disgust"))
fear <- length(which(es_2019$output_emotion == "fear"))
joy <- length(which(es_2019$output_emotion == "joy"))
#others <- length(which(es_2019$output_emotion == "others"))
sadness <- length(which(es_2019$output_emotion == "sadness"))
surprise <- length(which(es_2019$output_emotion == "surprise"))

Sentiment <- c("Anger","Disgust","Fear", "Sadness", "Surprise", "Joy")
Count <- c(anger, disgust, fear, sadness, surprise, joy)
output <- data.frame(Sentiment,Count)
output$Sentiment<-factor(output$Sentiment,levels=Sentiment)

ggplot(output, aes(x=Sentiment,y=Count))+
  geom_bar(alpha=0.7, stat = "identity", aes(fill = Sentiment)) +
  geom_text(aes(label = Count), vjust = -0.2) +
  scale_y_continuous(breaks = number_ticks(20)) +
  theme_minimal() +
  scale_fill_manual(values=palette_2) +
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
  labs(title = "Emotion type of tweets (2019)",
       subtitle = " ",
       x = " ",
       y = "Tweets",
       caption = paste(" ")) +
  ggsave(paste0("Figures/", "01", "_", "tweet_emotion_others_es_2019", ".jpeg"), 
         width = 10, height = 6)

#----ORIGINAL TWEETS (excluding "others") (2021)----

anger <- length(which(es_2021$output_emotion == "anger"))
disgust <- length(which(es_2021$output_emotion == "disgust"))
fear <- length(which(es_2021$output_emotion == "fear"))
joy <- length(which(es_2021$output_emotion == "joy"))
#others <- length(which(es_2021$output_emotion == "others"))
sadness <- length(which(es_2021$output_emotion == "sadness"))
surprise <- length(which(es_2021$output_emotion == "surprise"))

Sentiment <- c("Anger","Disgust","Fear", "Sadness", "Surprise", "Joy")
Count <- c(anger, disgust, fear, sadness, surprise, joy)
output <- data.frame(Sentiment,Count)
output$Sentiment<-factor(output$Sentiment,levels=Sentiment)

ggplot(output, aes(x=Sentiment,y=Count))+
  geom_bar(alpha=0.7, stat = "identity", aes(fill = Sentiment)) +
  geom_text(aes(label = Count), vjust = -0.2) +
  scale_y_continuous(breaks = number_ticks(20)) +
  theme_minimal() +
  scale_fill_manual(values=palette_2) +
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
  labs(title = "Emotion type of tweets (2021)",
       subtitle = " ",
       x = " ",
       y = "Tweets",
       caption = paste(" ")) +
  ggsave(paste0("Figures/", "01", "_", "tweet_emotion_others_es_2021", ".jpeg"), 
         width = 10, height = 6)

#----Tweets from conversations----

anger <- length(which(thread$output_emotion == "anger"))
disgust <- length(which(thread$output_emotion == "disgust"))
fear <- length(which(thread$output_emotion == "fear"))
joy <- length(which(thread$output_emotion == "joy"))
others <- length(which(thread$output_emotion == "others"))
sadness <- length(which(thread$output_emotion == "sadness"))
surprise <- length(which(thread$output_emotion == "surprise"))

Sentiment <- c("Anger","Disgust","Fear", "Sadness", "Surprise", "Joy", "Others")
Count <- c(anger, disgust, fear, sadness, surprise, joy, others)
output <- data.frame(Sentiment,Count)
output$Sentiment<-factor(output$Sentiment,levels=Sentiment)

ggplot(output, aes(x=Sentiment,y=Count))+
  geom_bar(alpha=0.7, stat = "identity", aes(fill = Sentiment)) +
  geom_text(aes(label = Count), vjust = -0.2) +
  scale_y_continuous(breaks = number_ticks(20)) +
  theme_minimal() +
  scale_fill_manual(values=palette_2) +
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
  labs(title = "Emotion type of tweets/replies",
       subtitle = " ",
       x = " ",
       y = "Tweets/replies",
       caption = paste(" ")) +
  ggsave(paste0("Figures/", "01", "_", "thread_emotion", ".jpeg"), 
         width = 10, height = 6)

#----Tweets from conversations (excluding "others")----

anger <- length(which(thread$output_emotion == "anger"))
disgust <- length(which(thread$output_emotion == "disgust"))
fear <- length(which(thread$output_emotion == "fear"))
joy <- length(which(thread$output_emotion == "joy"))
#others <- length(which(thread$output_emotion == "others"))
sadness <- length(which(thread$output_emotion == "sadness"))
surprise <- length(which(thread$output_emotion == "surprise"))

Sentiment <- c("Anger","Disgust","Fear", "Sadness", "Surprise", "Joy")
Count <- c(anger, disgust, fear, sadness, surprise, joy)
output <- data.frame(Sentiment,Count)
output$Sentiment<-factor(output$Sentiment,levels=Sentiment)

ggplot(output, aes(x=Sentiment,y=Count))+
  geom_bar(alpha=0.7, stat = "identity", aes(fill = Sentiment)) +
  geom_text(aes(label = Count), vjust = -0.2) +
  scale_y_continuous(breaks = number_ticks(20)) +
  theme_minimal() +
  scale_fill_manual(values=palette_2) +
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
  labs(title = "Emotion type of tweets/replies",
       subtitle = " ",
       x = " ",
       y = "Tweets/replies",
       caption = paste(" ")) +
  ggsave(paste0("Figures/", "01", "_", "thread_emotion_others", ".jpeg"), 
         width = 10, height = 6)


# *****************************************************************************
#### 06_Sentiments vs emotions ####
# *****************************************************************************

#----ORIGINAL TWEETS----

data_comp <-  tweet %>%
  filter(output_emotion != "others") %>% #Remove tweets whose emotion was classified as "others"
  count(output_emotion, output) %>%  #Get OUTPUT count. Order determines top or bottom.
  mutate(emotion=case_when(output_emotion=="anger" ~ "Anger",
                           output_emotion=="disgust" ~ "Disgust",
                           output_emotion=="fear" ~ "Fear",
                           output_emotion=="joy" ~ "Joy",
                           output_emotion=="others" ~ "Others",
                           output_emotion=="sadness" ~ "Sadness",
                           output_emotion=="surprise" ~ "Surprise"),
         sentiment=case_when(output=="POS" ~ "Positive",
                             output=="NEG" ~ "Negative",
                             output=="NEU" ~ "Neutral")) %>%
  select(emotion, sentiment, n)

circos.clear() #Very important - Reset the circular layout parameters!
grid.col = c("Anger" = palette_2[1], "Disgust" = palette_2[2], "Fear" = palette_2[3], "Joy" = palette_2[6], "Sadness" = palette_2[4], "Surprise" = palette_2[5], "Positive" = "grey30", "Negative" = "black", "Neutral" = "grey80") #assign chord colors
# Set the global parameters for the circular layout. Specifically the gap size
circos.par(gap.after = c(rep(6, length(unique(data_comp[[1]])) - 1), 15,
                         rep(6, length(unique(data_comp[[2]])) - 1), 15))

chordDiagram(data_comp, grid.col = grid.col, transparency = .2)
title("Relationship Between Sentiments and Emotions")

#----Tweets from conversations----

data_comp <-  thread %>%
  filter(output_emotion != "others") %>% #Remove tweets whose emotion was classified as "others"
  count(output_emotion, output) %>%  #Get OUTPUT count. Order determines top or bottom.
  mutate(emotion=case_when(output_emotion=="anger" ~ "Anger",
                           output_emotion=="disgust" ~ "Disgust",
                           output_emotion=="fear" ~ "Fear",
                           output_emotion=="joy" ~ "Joy",
                           output_emotion=="others" ~ "Others",
                           output_emotion=="sadness" ~ "Sadness",
                           output_emotion=="surprise" ~ "Surprise"),
         sentiment=case_when(output=="POS" ~ "Positive",
                             output=="NEG" ~ "Negative",
                             output=="NEU" ~ "Neutral")) %>%
  select(emotion, sentiment, n)

circos.clear() #Very important - Reset the circular layout parameters!
grid.col = c("Anger" = palette_2[1], "Disgust" = palette_2[2], "Fear" = palette_2[3], "Joy" = palette_2[6], "Sadness" = palette_2[4], "Surprise" = palette_2[5], "Positive" = "grey30", "Negative" = "black", "Neutral" = "grey80") #assign chord colors
# Set the global parameters for the circular layout. Specifically the gap size
circos.par(gap.after = c(rep(6, length(unique(data_comp[[1]])) - 1), 15,
                         rep(6, length(unique(data_comp[[2]])) - 1), 15))

chordDiagram(data_comp, grid.col = grid.col, transparency = .2)
title("Relationship Between Sentiments and Emotions (From tweets/replies)")


# *****************************************************************************
#### 07_Number of likes, retweets and replies by Senmtiment ####
# *****************************************************************************

#----ORIGINAL TWEETS----

sent <- c("Sentiment",
          "Positive",
          "Negative",
          "Neutral")

tweets <- c("Tweets",
           nrow(tweet[tweet$output=="POS",]), 
           nrow(tweet[tweet$output=="NEG",]), 
           nrow(tweet[tweet$output=="NEU",]))

likes <- c("Likes per tweet",
           round((sum(subset(tweet, output == "POS")$likes_count)/nrow(tweet[tweet$output=="POS",])),2),
           round((sum(subset(tweet, output == "NEG")$likes_count)/nrow(tweet[tweet$output=="NEG",])),2),
           round((sum(subset(tweet, output == "NEU")$likes_count)/nrow(tweet[tweet$output=="NEU",])),2))

replies <- c("Replies per tweet",
             round((sum(subset(tweet, output == "POS")$replies_count)/nrow(tweet[tweet$output=="POS",])),2),
             round((sum(subset(tweet, output == "NEG")$replies_count)/nrow(tweet[tweet$output=="NEG",])),2),
             round((sum(subset(tweet, output == "NEU")$replies_count)/nrow(tweet[tweet$output=="NEU",])),2))

retweets <- c("Retweets per tweet",
             round((sum(subset(tweet, output == "POS")$retweets_count)/nrow(tweet[tweet$output=="POS",])),2),
             round((sum(subset(tweet, output == "NEG")$retweets_count)/nrow(tweet[tweet$output=="NEG",])),2),
             round((sum(subset(tweet, output == "NEU")$retweets_count)/nrow(tweet[tweet$output=="NEU",])),2))

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
    save_kable("Figures/sum_stats_tweets_es.html")
  
  #----ORIGINAL TWEETS (2019)----
  
  sent <- c("Sentiment",
            "Positive",
            "Negative",
            "Neutral")
  
  tweets <- c("Tweets",
              nrow(es_2019[es_2019$output=="POS",]), 
              nrow(es_2019[es_2019$output=="NEG",]), 
              nrow(es_2019[es_2019$output=="NEU",]))
  
  likes <- c("Likes per tweet",
             round((sum(subset(es_2019, output == "POS")$likes_count)/nrow(es_2019[es_2019$output=="POS",])),2),
             round((sum(subset(es_2019, output == "NEG")$likes_count)/nrow(es_2019[es_2019$output=="NEG",])),2),
             round((sum(subset(es_2019, output == "NEU")$likes_count)/nrow(es_2019[es_2019$output=="NEU",])),2))
  
  replies <- c("Replies per tweet",
               round((sum(subset(es_2019, output == "POS")$replies_count)/nrow(es_2019[es_2019$output=="POS",])),2),
               round((sum(subset(es_2019, output == "NEG")$replies_count)/nrow(es_2019[es_2019$output=="NEG",])),2),
               round((sum(subset(es_2019, output == "NEU")$replies_count)/nrow(es_2019[es_2019$output=="NEU",])),2))
  
  retweets <- c("Retweets per tweet",
                round((sum(subset(es_2019, output == "POS")$rees_2019s_count)/nrow(es_2019[es_2019$output=="POS",])),2),
                round((sum(subset(es_2019, output == "NEG")$rees_2019s_count)/nrow(es_2019[es_2019$output=="NEG",])),2),
                round((sum(subset(es_2019, output == "NEU")$rees_2019s_count)/nrow(es_2019[es_2019$output=="NEU",])),2))
  
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
    save_kable("Figures/sum_stats_tweets_es_2019.html")

  #----ORIGINAL TWEETS (2021)----
  
  sent <- c("Sentiment",
            "Positive",
            "Negative",
            "Neutral")
  
  tweets <- c("Tweets",
              nrow(es_2021[es_2021$output=="POS",]), 
              nrow(es_2021[es_2021$output=="NEG",]), 
              nrow(es_2021[es_2021$output=="NEU",]))
  
  likes <- c("Likes per tweet",
             round((sum(subset(es_2021, output == "POS")$likes_count)/nrow(es_2021[es_2021$output=="POS",])),2),
             round((sum(subset(es_2021, output == "NEG")$likes_count)/nrow(es_2021[es_2021$output=="NEG",])),2),
             round((sum(subset(es_2021, output == "NEU")$likes_count)/nrow(es_2021[es_2021$output=="NEU",])),2))
  
  replies <- c("Replies per tweet",
               round((sum(subset(es_2021, output == "POS")$replies_count)/nrow(es_2021[es_2021$output=="POS",])),2),
               round((sum(subset(es_2021, output == "NEG")$replies_count)/nrow(es_2021[es_2021$output=="NEG",])),2),
               round((sum(subset(es_2021, output == "NEU")$replies_count)/nrow(es_2021[es_2021$output=="NEU",])),2))
  
  retweets <- c("Retweets per tweet",
                round((sum(subset(es_2021, output == "POS")$rees_2021s_count)/nrow(es_2021[es_2021$output=="POS",])),2),
                round((sum(subset(es_2021, output == "NEG")$rees_2021s_count)/nrow(es_2021[es_2021$output=="NEG",])),2),
                round((sum(subset(es_2021, output == "NEU")$rees_2021s_count)/nrow(es_2021[es_2021$output=="NEU",])),2))
  
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
    save_kable("Figures/sum_stats_tweets_es_2021.html")
  
# *****************************************************************************
#### 08_Number of likes, retweets and replies by Emotion ####
# *****************************************************************************
 
   #----ORIGINAL TWEETS ----
  
  emot <- c("Anger",
            "Disgust",
            "Fear",
            "Sadness",
            "Surprise",
            "Joy",
            "Others")
  
  tweets <- c(nrow(tweet[tweet$output_emotion=="anger",]), 
              nrow(tweet[tweet$output_emotion=="disgust",]), 
              nrow(tweet[tweet$output_emotion=="fear",]), 
              nrow(tweet[tweet$output_emotion=="sadness",]), 
              nrow(tweet[tweet$output_emotion=="surprise",]), 
              nrow(tweet[tweet$output_emotion=="joy",]), 
              nrow(tweet[tweet$output_emotion=="others",]))
  
  likes <- c(round((sum(subset(tweet, output_emotion == "anger")$likes_count)/nrow(tweet[tweet$output_emotion=="anger",])),2),
             round((sum(subset(tweet, output_emotion == "disgust")$likes_count)/nrow(tweet[tweet$output_emotion=="disgust",])),2),
             round((sum(subset(tweet, output_emotion == "fear")$likes_count)/nrow(tweet[tweet$output_emotion=="fear",])),2),
             round((sum(subset(tweet, output_emotion == "sadness")$likes_count)/nrow(tweet[tweet$output_emotion=="sadness",])),2),
             round((sum(subset(tweet, output_emotion == "surprise")$likes_count)/nrow(tweet[tweet$output_emotion=="surprise",])),2),
             round((sum(subset(tweet, output_emotion == "joy")$likes_count)/nrow(tweet[tweet$output_emotion=="joy",])),2),
             round((sum(subset(tweet, output_emotion == "others")$likes_count)/nrow(tweet[tweet$output_emotion=="others",])),2))
  
  replies <- c(round((sum(subset(tweet, output_emotion == "anger")$replies_count)/nrow(tweet[tweet$output_emotion=="anger",])),2),
               round((sum(subset(tweet, output_emotion == "disgust")$replies_count)/nrow(tweet[tweet$output_emotion=="disgust",])),2),
               round((sum(subset(tweet, output_emotion == "fear")$replies_count)/nrow(tweet[tweet$output_emotion=="fear",])),2),
               round((sum(subset(tweet, output_emotion == "sadness")$replies_count)/nrow(tweet[tweet$output_emotion=="sadness",])),2),
               round((sum(subset(tweet, output_emotion == "surprise")$replies_count)/nrow(tweet[tweet$output_emotion=="surprise",])),2),
               round((sum(subset(tweet, output_emotion == "joy")$replies_count)/nrow(tweet[tweet$output_emotion=="joy",])),2),
               round((sum(subset(tweet, output_emotion == "others")$replies_count)/nrow(tweet[tweet$output_emotion=="others",])),2))
  
  retweets <- c(round((sum(subset(tweet, output_emotion == "anger")$retweets_count)/nrow(tweet[tweet$output_emotion=="anger",])),2),
                round((sum(subset(tweet, output_emotion == "disgust")$retweets_count)/nrow(tweet[tweet$output_emotion=="disgust",])),2),
                round((sum(subset(tweet, output_emotion == "fear")$retweets_count)/nrow(tweet[tweet$output_emotion=="fear",])),2),
                round((sum(subset(tweet, output_emotion == "sadness")$retweets_count)/nrow(tweet[tweet$output_emotion=="sadness",])),2),
                round((sum(subset(tweet, output_emotion == "surprise")$retweets_count)/nrow(tweet[tweet$output_emotion=="surprise",])),2),
                round((sum(subset(tweet, output_emotion == "joy")$retweets_count)/nrow(tweet[tweet$output_emotion=="joy",])),2),
                round((sum(subset(tweet, output_emotion == "others")$retweets_count)/nrow(tweet[tweet$output_emotion=="others",])),2))
  
  table_2 <- data.frame(emot, as.character(tweets), as.character(likes), as.character(replies), as.character(retweets))
  table_2 <- rename(table_2, "Emotion" = emot)
  table_2 <- rename(table_2, "Number of tweets" = as.character.tweets.)
  table_2 <- rename(table_2, "Likes per tweet" = as.character.likes.)
  table_2 <- rename(table_2, "Replies per tweet" = as.character.replies.)
  table_2 <- rename(table_2, "Retweets per tweet" = as.character.retweets.)
  
  dust(table_2) %>%
    kable(align = "lllll",
          format = "html") %>%
    kable_styling() %>%
    save_kable("Figures/sum_stats_emotions_sp.html")
  
  #----ORIGINAL TWEETS (2019)----
  
  emot <- c("Anger",
            "Disgust",
            "Fear",
            "Sadness",
            "Surprise",
            "Joy",
            "Others")
  
  tweets <- c(nrow(es_2019[es_2019$output_emotion=="anger",]), 
              nrow(es_2019[es_2019$output_emotion=="disgust",]), 
              nrow(es_2019[es_2019$output_emotion=="fear",]), 
              nrow(es_2019[es_2019$output_emotion=="sadness",]), 
              nrow(es_2019[es_2019$output_emotion=="surprise",]), 
              nrow(es_2019[es_2019$output_emotion=="joy",]), 
              nrow(es_2019[es_2019$output_emotion=="others",]))
  
  likes <- c(round((sum(subset(es_2019, output_emotion == "anger")$likes_count)/nrow(es_2019[es_2019$output_emotion=="anger",])),2),
             round((sum(subset(es_2019, output_emotion == "disgust")$likes_count)/nrow(es_2019[es_2019$output_emotion=="disgust",])),2),
             round((sum(subset(es_2019, output_emotion == "fear")$likes_count)/nrow(es_2019[es_2019$output_emotion=="fear",])),2),
             round((sum(subset(es_2019, output_emotion == "sadness")$likes_count)/nrow(es_2019[es_2019$output_emotion=="sadness",])),2),
             round((sum(subset(es_2019, output_emotion == "surprise")$likes_count)/nrow(es_2019[es_2019$output_emotion=="surprise",])),2),
             round((sum(subset(es_2019, output_emotion == "joy")$likes_count)/nrow(es_2019[es_2019$output_emotion=="joy",])),2),
             round((sum(subset(es_2019, output_emotion == "others")$likes_count)/nrow(es_2019[es_2019$output_emotion=="others",])),2))
  
  replies <- c(round((sum(subset(es_2019, output_emotion == "anger")$replies_count)/nrow(es_2019[es_2019$output_emotion=="anger",])),2),
               round((sum(subset(es_2019, output_emotion == "disgust")$replies_count)/nrow(es_2019[es_2019$output_emotion=="disgust",])),2),
               round((sum(subset(es_2019, output_emotion == "fear")$replies_count)/nrow(es_2019[es_2019$output_emotion=="fear",])),2),
               round((sum(subset(es_2019, output_emotion == "sadness")$replies_count)/nrow(es_2019[es_2019$output_emotion=="sadness",])),2),
               round((sum(subset(es_2019, output_emotion == "surprise")$replies_count)/nrow(es_2019[es_2019$output_emotion=="surprise",])),2),
               round((sum(subset(es_2019, output_emotion == "joy")$replies_count)/nrow(es_2019[es_2019$output_emotion=="joy",])),2),
               round((sum(subset(es_2019, output_emotion == "others")$replies_count)/nrow(es_2019[es_2019$output_emotion=="others",])),2))
  
  retweets <- c(round((sum(subset(es_2019, output_emotion == "anger")$rees_2019s_count)/nrow(es_2019[es_2019$output_emotion=="anger",])),2),
                round((sum(subset(es_2019, output_emotion == "disgust")$rees_2019s_count)/nrow(es_2019[es_2019$output_emotion=="disgust",])),2),
                round((sum(subset(es_2019, output_emotion == "fear")$rees_2019s_count)/nrow(es_2019[es_2019$output_emotion=="fear",])),2),
                round((sum(subset(es_2019, output_emotion == "sadness")$rees_2019s_count)/nrow(es_2019[es_2019$output_emotion=="sadness",])),2),
                round((sum(subset(es_2019, output_emotion == "surprise")$rees_2019s_count)/nrow(es_2019[es_2019$output_emotion=="surprise",])),2),
                round((sum(subset(es_2019, output_emotion == "joy")$rees_2019s_count)/nrow(es_2019[es_2019$output_emotion=="joy",])),2),
                round((sum(subset(es_2019, output_emotion == "others")$rees_2019s_count)/nrow(es_2019[es_2019$output_emotion=="others",])),2))
  
  table_2 <- data.frame(emot, as.character(tweets), as.character(likes), as.character(replies), as.character(retweets))
  table_2 <- rename(table_2, "Emotion" = emot)
  table_2 <- rename(table_2, "Number of tweets" = as.character.tweets.)
  table_2 <- rename(table_2, "Likes per tweet" = as.character.likes.)
  table_2 <- rename(table_2, "Replies per tweet" = as.character.replies.)
  table_2 <- rename(table_2, "Retweets per tweet" = as.character.retweets.)
  
  dust(table_2) %>%
    kable(align = "lllll",
          format = "html") %>%
    kable_styling() %>%
    save_kable("Figures/sum_stats_emotions_sp_2019.html")
  
  #----ORIGINAL TWEETS (2021)----
  
  emot <- c("Anger",
            "Disgust",
            "Fear",
            "Sadness",
            "Surprise",
            "Joy",
            "Others")
  
  tweets <- c(nrow(es_2021[es_2021$output_emotion=="anger",]), 
              nrow(es_2021[es_2021$output_emotion=="disgust",]), 
              nrow(es_2021[es_2021$output_emotion=="fear",]), 
              nrow(es_2021[es_2021$output_emotion=="sadness",]), 
              nrow(es_2021[es_2021$output_emotion=="surprise",]), 
              nrow(es_2021[es_2021$output_emotion=="joy",]), 
              nrow(es_2021[es_2021$output_emotion=="others",]))
  
  likes <- c(round((sum(subset(es_2021, output_emotion == "anger")$likes_count)/nrow(es_2021[es_2021$output_emotion=="anger",])),2),
             round((sum(subset(es_2021, output_emotion == "disgust")$likes_count)/nrow(es_2021[es_2021$output_emotion=="disgust",])),2),
             round((sum(subset(es_2021, output_emotion == "fear")$likes_count)/nrow(es_2021[es_2021$output_emotion=="fear",])),2),
             round((sum(subset(es_2021, output_emotion == "sadness")$likes_count)/nrow(es_2021[es_2021$output_emotion=="sadness",])),2),
             round((sum(subset(es_2021, output_emotion == "surprise")$likes_count)/nrow(es_2021[es_2021$output_emotion=="surprise",])),2),
             round((sum(subset(es_2021, output_emotion == "joy")$likes_count)/nrow(es_2021[es_2021$output_emotion=="joy",])),2),
             round((sum(subset(es_2021, output_emotion == "others")$likes_count)/nrow(es_2021[es_2021$output_emotion=="others",])),2))
  
  replies <- c(round((sum(subset(es_2021, output_emotion == "anger")$replies_count)/nrow(es_2021[es_2021$output_emotion=="anger",])),2),
               round((sum(subset(es_2021, output_emotion == "disgust")$replies_count)/nrow(es_2021[es_2021$output_emotion=="disgust",])),2),
               round((sum(subset(es_2021, output_emotion == "fear")$replies_count)/nrow(es_2021[es_2021$output_emotion=="fear",])),2),
               round((sum(subset(es_2021, output_emotion == "sadness")$replies_count)/nrow(es_2021[es_2021$output_emotion=="sadness",])),2),
               round((sum(subset(es_2021, output_emotion == "surprise")$replies_count)/nrow(es_2021[es_2021$output_emotion=="surprise",])),2),
               round((sum(subset(es_2021, output_emotion == "joy")$replies_count)/nrow(es_2021[es_2021$output_emotion=="joy",])),2),
               round((sum(subset(es_2021, output_emotion == "others")$replies_count)/nrow(es_2021[es_2021$output_emotion=="others",])),2))
  
  retweets <- c(round((sum(subset(es_2021, output_emotion == "anger")$rees_2021s_count)/nrow(es_2021[es_2021$output_emotion=="anger",])),2),
                round((sum(subset(es_2021, output_emotion == "disgust")$rees_2021s_count)/nrow(es_2021[es_2021$output_emotion=="disgust",])),2),
                round((sum(subset(es_2021, output_emotion == "fear")$rees_2021s_count)/nrow(es_2021[es_2021$output_emotion=="fear",])),2),
                round((sum(subset(es_2021, output_emotion == "sadness")$rees_2021s_count)/nrow(es_2021[es_2021$output_emotion=="sadness",])),2),
                round((sum(subset(es_2021, output_emotion == "surprise")$rees_2021s_count)/nrow(es_2021[es_2021$output_emotion=="surprise",])),2),
                round((sum(subset(es_2021, output_emotion == "joy")$rees_2021s_count)/nrow(es_2021[es_2021$output_emotion=="joy",])),2),
                round((sum(subset(es_2021, output_emotion == "others")$rees_2021s_count)/nrow(es_2021[es_2021$output_emotion=="others",])),2))
  
  table_2 <- data.frame(emot, as.character(tweets), as.character(likes), as.character(replies), as.character(retweets))
  table_2 <- rename(table_2, "Emotion" = emot)
  table_2 <- rename(table_2, "Number of tweets" = as.character.tweets.)
  table_2 <- rename(table_2, "Likes per tweet" = as.character.likes.)
  table_2 <- rename(table_2, "Replies per tweet" = as.character.replies.)
  table_2 <- rename(table_2, "Retweets per tweet" = as.character.retweets.)
  
  dust(table_2) %>%
    kable(align = "lllll",
          format = "html") %>%
    kable_styling() %>%
    save_kable("Figures/sum_stats_emotions_sp_2021.html")
  
  