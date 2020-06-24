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

Tweet_df<- read.csv("C:\\path\\") 

# extracting information about users through Twitter API, 90000 observations every 15 minutes
###for the first time
users_info1<- lookup_users(Tweet_df$user_id)
###from the second onwards 
users_info2<- lookup_users(users$user_id)
#... 

users_Tweet_df <- users_info1 %>% 
  bind_rows(users_info2) %>%
  bind_rows(users_info3) %>%
  bind_rows(users_info4) %>%
  bind_rows(users_info5) %>%
  bind_rows(users_info6) 


Tweet_df<- Tweet_df %>%
  mutate(date = (gsub("T"," ", created_at))) %>%
  mutate(date = (gsub("Z"," ", date))) %>%
  mutate(date = as.POSIXct(date, format = "%Y-%m-%d %H:%M:%S")) 

df_Tweet_users<- Tweet_df%>% left_join(users_Tweet_df, by= "screen_name") 

###
# Sys.setlocale("LC_TIME", "English") 
# 
# Tweet_df_selected_2 <- Tweet_df_selected_2 %>%
#   mutate(date = as.POSIXct(Tweet.Posted.Time..UTC., format = "%d %b %Y %H:%M:%S")) 
###


Tweet_df_selected <- df_Tweet_users %>% select(screen_name, text, lang, location, date)

text<- c() 
text<- Tweet_df_selected$text
hashtags <- str_extract_all(text, "#\\S+")
str(hashtags)
Tweet_df_selected <- Tweet_df_selected %>% mutate(hashtags = hashtags) 

hashtags <- unlist(hashtags)
df<- data.frame(c(hashtags)) 
df <- df %>% group_by(c.hashtags.) %>% count(c.hashtags.)
df <- df  %>% arrange(desc(n))

write.csv(df,"C:\\path...\\count_hashtag.csv") 

###Time interval 1 
start_date <- as.POSIXct('2019-12-12 14:12:32', tz="UTC")
end_date <- as.POSIXct("2019-12-31 18:09:35") 
Tweet_df_finestra1<- Tweet_df_selected %>% 
  filter(strftime(date) > start_date & date <= end_date)

###Time interval 2
start_date <- as.POSIXct('2020-01-01 00:00:00', tz="UTC")
end_date <- as.POSIXct("2020-01-15 23:59:59") 
Tweet_df_finestra2<- Tweet_df_selected %>% 
  filter(strftime(date) > start_date & date <= end_date)

###Time interval 3
start_date <- as.POSIXct('2020-01-16 00:00:00', tz="UTC")
end_date <- as.POSIXct("2020-01-31 23:59:59") 
Tweet_df_finestra3<- Tweet_df_selected %>% 
  filter(strftime(date) > start_date & date <= end_date)

###Time interval 4
start_date <- as.POSIXct('2020-02-01 00:00:00', tz="UTC")
end_date <- as.POSIXct("2020-02-15 23:59:59") 
Tweet_df_finestra4<- Tweet_df_selected %>% 
  filter(strftime(date) > start_date & date <= end_date)

###Time interval 5
start_date <- as.POSIXct('2020-02-16 00:00:00', tz="UTC")
end_date <- as.POSIXct("2020-02-29 23:59:59") 
Tweet_df_finestra5<- Tweet_df_selected %>% 
  filter(strftime(date) > start_date & date <= end_date) 

###Time interval 6
start_date <- as.POSIXct('2020-03-01 00:00:00', tz="UTC")
end_date <- as.POSIXct("2020-03-15 23:59:59") 
Tweet_df_finestra6<- Tweet_df_selected %>% 
  filter(strftime(date) > start_date & date <= end_date)

###Time interval 7
start_date <- as.POSIXct('2020-03-16 00:00:00', tz="UTC")
end_date <- as.POSIXct("2020-03-30 23:59:59") 
Tweet_df_finestra7<- Tweet_df_selected %>% 
  filter(strftime(date) > start_date & date <= end_date)


