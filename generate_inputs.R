if(T){
  rm(list=ls())
  if (!require("readr")) install.packages("readr")
  library(readr)
  setwd("~/MEGA/Projeto-Mestrado/Artigo-2")
  
  #if (!require("readr")) install.packages("readr")
  #library(readr)
  if (!require("tm")) install.packages("tm")
  library(tm)
  if (!require("topicmodels")) install.packages("topicmodels")
  library(topicmodels)
  if (!require("tidytext")) install.packages("tidytext")
  library(tidytext)
  if (!require("ggplot2")) install.packages("ggplot2")
  library(ggplot2)
  if (!require("dplyr")) install.packages("dplyr")
  library(dplyr)
  if (!require("stringr")) install.packages("stringr")
  library(stringr)  
  if (!require("ggpubr")) install.packages("ggpubr")
  if (!require("tidyr")) install.packages("tidyr")
  library(tidyr)    
  
  #Get Datasets
  l_tweets <- read_csv("~/MEGA/Projeto-Mestrado/Data/dataset.csv")
  #l_tweets <- read_csv("~/MEGA/Projeto-Mestrado/R-Project/Data/s01.csv")
  l_tweets$X1 <- NULL
  
  #Tradução dos valores#  
  l_tweets[l_tweets[, "Campaign"] == "Outubro Rosa", ]$Campaign <- 'Pink October'
  l_tweets[l_tweets[, "Campaign"] == "Novembro Azul", ]$Campaign <- 'Blue November'
  l_tweets[is.na(l_tweets[, "gender"]), ]$gender <- 'N/D'
  #l_tweets[l_tweets[, "gender"] == "Organizao", ]$gender <- 'Organizao'
  l_tweets[l_tweets[, "gender"] == "Organizao", ]$gender <- 'Organization'
  
  #Remoção de linhas
  l_tweets <- l_tweets[!(l_tweets$Campaign %in% 'Pink October' & l_tweets$tweet_created_date %in% 'Nov 07 2017'), ]
  
  #p_campaign <- 'Blue November'
  p_campaign <- 'Pink October'
  
}

setwd("~/MEGA/Projeto-Mestrado/Artigo3/Topic-Model-Twitter/data/lda-result-topics")

l_tweets <- l_tweets[l_tweets$language == "english", ]
tweets <- l_tweets
tweets <- tweets[tweets[, "Campaign"] == p_campaign, ]
tweets <- unique(tweets[ , c("screenName", "text", "created.y", "id.y")])
l_dataset <- tweets
tweets <- str_replace_all(tweets$text, "[^[:alnum:]]", " ")
tweets <- str_replace_all(tweets,"[^[:graph:]]", " ") 
tweets <- tolower(tweets)



#Remove stopwords
all_stops <- c("https", "RT", "movember", "breastcancerawarenessmonth", 
               "breastcancerawareness", "breastcancer", "americancancer",
               "movembercanada", "prostatecancer", "htt", "http", 
               "outubrorosa", "novembroazul", "octubrerosa", "&amp")

all_stops <- c(all_stops, stopwords("en"))
tweets <- removeWords(tweets, all_stops)
tweets <- stemDocument(x = tweets, language = "en")



#Convert data frame into a DocumentTermMatrix List
tweets <- Corpus(VectorSource(tweets))  
corpus <- tweets
tweets <- DocumentTermMatrix(tweets)
dtm <- tweets
ui = unique(tweets$i)
tweets = tweets[ui,]
l_dataset <- l_dataset[ui,]



###########################
# FINAL                   #
###########################
setwd("~/Documents/DSProjects/Palmetto-from-R")
source("/home/roberto/Documents/DSProjects/PinkOctoberMovember-Analysis/lda.R")
p_campaign <- "BN-2014"
dtm <- get_dtm(p_campaign)
setwd("~/Documents/DSProjects/Palmetto-from-R")

for(i in 2:17){
  # set a seed so that the output of the model is predictable
  ap_lda <- LDA(dtm, k = i, method="Gibbs", control = list(seed = 1234))
  
  ap_topics <- tidy(ap_lda, matrix = "beta")
  ap_topics <- ap_topics %>%
    group_by(topic) %>%
    top_n(10, beta) %>%
    ungroup() %>%
    arrange(topic, -beta)
  
  #Get top 15 topics
  top_topics <- aggregate(ap_topics$beta, by=list(Category=ap_topics$topic), FUN=sum)
  top_topics <- top_topics[order(-top_topics$x),][1:15,]
  top_topics$x <- NULL
  ap_topics <- ap_topics[(ap_topics$topic %in% top_topics$Category),]
  
  #Arranja as palavras para dar entrada no Palmetto
  ap_topics$beta <- NULL
  ap_topics$seq <- 0
  ap_topics$seq <- ave(ap_topics$term, ap_topics$topic, FUN = seq_along)
  terms_online  <- spread(ap_topics, seq, term)
  terms_online$topic <- NULL
  
  write.table(terms_online, gsub(" ", "", paste("final_results/", p_campaign, "-", i, "-final.csv")), row.names = FALSE, col.names=FALSE, quote = FALSE)    
  
}

