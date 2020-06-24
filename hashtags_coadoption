library(stringr)
library(maps)
library(qdapRegex)
library(dplyr)
library(ggplot2)
library(tm)
library(qdap)
library(wordcloud)
library(rtweet) 
library(httpuv)
library(reshape)
library(tidyverse)
library(tidyr)
library(data.table)
library(igraph)
library(lubridate)

Dataset <- read.csv("C:\\path\\") 
sampled_users <- read.csv("C:\\path...\\sampled_users.csv") 


sampled_dataset <- sampled_users%>% left_join(Dataset, by= "screen_name") 

#DIVIDO LA LISTA DI HASHTAGS PER COLONNE (ripeto per tutti i datasets)
sampled_dataset <- sampled_dataset%>% filter(hashtags != "NULL")
for (i in 1:length(sampled_dataset_1$screen_name)) 
{
  for (j in 1: max(lengths(sampled_dataset_1$hashtags))){
    
    sampled_dataset_1[i,9+j]<- tolower(sampled_dataset_1$hashtags[[i]][j])
  }}

hashtag_dataset <- sampled_dataset[10:50]
hashtag_dataset <- hashtag_dataset %>%  mutate_all(funs(str_replace(., "#", ""))) %>%
  mutate_all(funs(str_replace(., "\\.", ""))) %>%
  mutate_all(funs(str_replace(., "\\;", ""))) %>%
  mutate_all(funs(str_replace(., "\\:", ""))) %>%
  mutate_all(funs(str_replace(., "\\)", ""))) %>% 
  mutate_all(funs(str_replace(., "\\?", ""))) %>%
  mutate_all(funs(str_replace(., "\\!", ""))) %>%
  mutate_all(funs(str_replace(., "\\,", "")))




## edgelist 
matrix <- as.matrix(hashtag_dataset) 
write.table(matrix, "C:\\path...\\coadoption_matrix_usa.csv" , row.names = FALSE, col.names = FALSE)
