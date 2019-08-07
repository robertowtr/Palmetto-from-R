rm(list=ls())
library(readr)
library(tm)
library(topicmodels)
library(tidytext)
library(ggplot2)
library(dplyr)
library(stringr)  
library(tidyr)    

setwd("/Users/roberto.walter/Documents/code_source/github/mestrado-stuff/PinkOctoberMovember-Analysis-master/")
source("/Users/roberto.walter/Documents/code_source/github/mestrado-stuff/PinkOctoberMovember-Analysis-master/lda.R")
p_campaign <- "BN"
p_year <- 2017

print(paste("Starting works on", p_campaign, "campaign!"))
dtm <- get_dtm(p_campaign, p_year)
setwd("~/Documents/code_source/github/Palmetto-from-R")

print("Starting to generate LDA results")
for(i in 2:17){
  print(paste("Working on", i, "topic! :D"))
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
  
  write.table(terms_online, gsub(" ", "", paste("final_results/", p_campaign, "/", p_campaign, "-", i, "-final.csv")), row.names = FALSE, col.names=FALSE, quote = FALSE)    
}



#Set working directory
output_file <- data.frame(x = character(), y = numeric())

print("Getting files")
file.names <- list.files(path = gsub(" ", "", paste('final_results/', p_campaign, "/")))
file <- "BN-3-final.csv"
for(file in file.names){
  print(paste("Working on", file))
  command = paste("./palmetto.sh", "C_V", gsub(" ", "", paste("/Users/roberto.walter/Documents/code_source/github/Palmetto-from-R/final_results/", p_campaign, "/", file)))
  result = system(command,intern=TRUE)  
  
  sum <- 0
  for (line in (2:length(result))){
    x <- strsplit(result[line],"\t")  
    x <- as.numeric(x[[1]][2])
    sum <- sum + x
  }
  sum <- sum / (length(result) - 1)  
  output_file <- rbind(output_file, data.frame(x = file, y = sum))
}

write.table(output_file, file = paste(p_campaign, "output.txt"), quote = FALSE, col.names = TRUE, row.names = FALSE)

print("Starting on Palmetto")
palmetto_input <- output_file

library(magrittr)
library(dplyr)

palmetto_input$x <- gsub(p_campaign, "", palmetto_input$x)
palmetto_input$x <- gsub("final", "", palmetto_input$x)
palmetto_input$x <- gsub("-", "", palmetto_input$x)
palmetto_input$x <- gsub(".csv", "", palmetto_input$x)

top_topics <- palmetto_input %>%
  top_n(3, y) %>%
  ungroup() %>%
  arrange(x, y)

library(ggplot2)
top <- ggplot(data = palmetto_input) + 
  geom_point(mapping = aes(x = x, y = y)) +
  geom_point(data = top_topics, mapping = aes(x = x, y = y, color = "red")) +
  expand_limits(y = 0)
top
