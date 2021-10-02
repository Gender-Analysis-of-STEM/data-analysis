#*****************************************************************************# 
# Purpose: Plot time series of tweets, likes and retweets                     #
# Database: Second selection & Only Spanish                                   #
#                                                                             #
# Created: October 01, 2021                                                   #
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


# *****************************************************************************
#### 02_Set directories ####
# *****************************************************************************
setwd("../../../../Dropbox/F2BD Literature Review/")

# *****************************************************************************
#### 03_Load_data ####
# *****************************************************************************
tweet <- read.csv("sentiment-emotion-analysis-sp.csv")
es_2019 <- tweet %>%
  filter(File=="ES_2019.csv")
es_2021 <- tweet %>%
  filter(File=="ES_2021.csv")

# *****************************************************************************
#### 04_Number of tweets ####
# *****************************************************************************

dt_2019 <- as.data.frame(table(es_2019$date))
dt_2021 <- as.data.frame(table(es_2021$date))

dt_2019<-dt_2019[!(dt_2019$Freq==0),]
dt_2021<-dt_2021[!(dt_2021$Freq==0),]

dt_2019 <- rename(dt_2019, Date = Var1)
dt_2021 <- rename(dt_2021, Date = Var1)

dt_2019$Proportion <- round(((dt_2019$Freq/sum(dt_2019$Freq))*100),2)
dt_2021$Proportion <- round(((dt_2021$Freq/sum(dt_2021$Freq))*100),2)

# Switch to time-based variables
dt_2019$Date <- as.Date(dt_2019$Date)
dt_2021$Date <- as.Date(dt_2021$Date)

# Switch to XTS format
dt_2019_freq <- xts(x = dt_2019$Freq, order.by = dt_2019$Date)
dt_2021_freq <- xts(x = dt_2021$Freq, order.by = dt_2021$Date)

dt_2019_prop <- xts(x = dt_2019$Proportion, order.by = dt_2019$Date)
dt_2021_prop <- xts(x = dt_2021$Proportion, order.by = dt_2021$Date)

# Number of Tweets 2019
freq_2019 <- dygraph(dt_2019_freq, main = "Number of Tweets, 2019") %>%
  dyOptions( stepPlot=TRUE,
             fillGraph=TRUE,
             colors = "#650c7d",
             axisLabelFontSize = 10,
             drawGrid = TRUE,
             gridLineColor = "grey") %>%
  dyAxis(name = "y",
         label = "Number of tweets")
         
         # Proportion of Tweets 2019
         prop_2019 <- dygraph(dt_2019_prop, main = "Proportion of tweets per day, 2019") %>%
           dyOptions( stepPlot=TRUE,
                      fillGraph=TRUE,
                      colors = "#650c7d",
                      axisLabelFontSize = 10,
                      drawGrid = TRUE,
                      gridLineColor = "grey") %>%
           dyAxis(name = "y",
                  label = "%")
         
# Number of Tweets 2021
freq_2021 <- dygraph(dt_2021_freq, main = "Number of Tweets, 2021") %>%
  dyOptions( stepPlot=TRUE,
              fillGraph=TRUE,
              colors = "#0e888f",
              axisLabelFontSize = 10,
              drawGrid = TRUE,
              gridLineColor = "grey") %>%
  dyAxis(name = "y",
        label = "Number of tweets")
         
         # Proportion of Tweets 2019
         prop_2021 <- dygraph(dt_2021_prop, main = "Proportion of tweets per day, 2021") %>%
           dyOptions( stepPlot=TRUE,
                      fillGraph=TRUE,
                      colors = "#0e888f",
                      axisLabelFontSize = 10,
                      drawGrid = TRUE,
                      gridLineColor = "grey") %>%
           dyAxis(name = "y",
                  label = "%")
         
# *****************************************************************************
#### 04_retweets ####
# *****************************************************************************
rt_2019 <- rbind(es_2019)
rt_2019 <- rt_2019 %>%
 select(date, retweets_count) %>%
 rename(Date = date,
        Retweet = retweets_count) %>%
 group_by(Date) %>%
 summarize(mean(Retweet)) %>%
 rename(Retweets = "mean(Retweet)")

rt_2021 <- rbind(es_2021)
rt_2021 <- rt_2021 %>%
 select(date, retweets_count) %>%
 rename(Date = date,
        Retweet = retweets_count) %>%
 group_by(Date) %>%
 summarize(mean(Retweet)) %>%
 rename(Retweets = "mean(Retweet)")
         
# Switch to time-based variables
rt_2019$Date <- as.Date(rt_2019$Date)
rt_2021$Date <- as.Date(rt_2021$Date)

rt_2019 <- xts(x = rt_2019$Retweets, order.by = rt_2019$Date)
rt_2021 <- xts(x = rt_2021$Retweets, order.by = rt_2021$Date)


# Mean of retweets, 2019
retweet_2019 <- dygraph(rt_2019, main = "Retweets (mean), 2019") %>%
  dyOptions( stepPlot=TRUE,
             fillGraph=TRUE,
             colors = "#650c7d",
             axisLabelFontSize = 10,
             drawGrid = TRUE,
             gridLineColor = "grey") %>%
  dyAxis(name = "y",
         label = "Retweets (mean)") 

# Mean of retweets, 2021
retweet_2021 <- dygraph(rt_2021, main = "Retweets (mean), 2021") %>%
  dyOptions( stepPlot=TRUE,
             fillGraph=TRUE,
             colors = "#0e888f",
             axisLabelFontSize = 10,
             drawGrid = TRUE,
             gridLineColor = "grey") %>%
  dyAxis(name = "y",
         label = "Retweets (mean)") 


# *****************************************************************************
#### 04_likes ####
# *****************************************************************************
lk_2019 <- rbind(es_2019)
lk_2019 <- lk_2019 %>%
  select(date, likes_count) %>%
  rename(Date = date,
         Likes = likes_count) %>%
  group_by(Date) %>%
  summarize(mean(Likes)) %>%
  rename(Likes = "mean(Likes)")

lk_2021 <- rbind(es_2021)
lk_2021 <- lk_2021 %>%
  select(date, likes_count) %>%
  rename(Date = date,
         Likes = likes_count) %>%
  group_by(Date) %>%
  summarize(mean(Likes)) %>%
  rename(Likes = "mean(Likes)")

# Switch to time-based variables
lk_2019$Date <- as.Date(lk_2019$Date)
lk_2021$Date <- as.Date(lk_2021$Date)

lk_2019 <- xts(x = lk_2019$Likes, order.by = lk_2019$Date)
lk_2021 <- xts(x = lk_2021$Likes, order.by = lk_2021$Date)


# Mean of likes, 2019
like_2019 <- dygraph(lk_2019, main = "Likes (mean), 2019") %>%
  dyOptions( stepPlot=TRUE,
             fillGraph=TRUE,
             colors = "#650c7d",
             axisLabelFontSize = 10,
             drawGrid = TRUE,
             gridLineColor = "grey") %>%
  dyAxis(name = "y",
         label = "Likes (mean)") 

# Mean of likes, 2021
like_2021 <- dygraph(rt_2021, main = "Likes (mean), 2021") %>%
  dyOptions( stepPlot=TRUE,
             fillGraph=TRUE,
             colors = "#0e888f",
             axisLabelFontSize = 10,
             drawGrid = TRUE,
             gridLineColor = "grey") %>%
  dyAxis(name = "y",
         label = "Likes (mean)") 