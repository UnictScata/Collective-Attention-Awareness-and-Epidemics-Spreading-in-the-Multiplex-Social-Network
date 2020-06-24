library( gtrendsR ) 

# google search

time = "2020-03-16 2020-03-31"
channel = 'web' 
#carico il data frame
rel_query_df<- read.csv("path.../16Mar-31Mar_relatedQueries_CINA.csv", header = TRUE, sep = ",", row.names=NULL)


keyword<-rel_query_df[2:6,1]
keyword_1<-rel_query_df[7:11,1]
keyword_2<-rel_query_df[12:16,1]
keyword_3<-rel_query_df[17:21,1]
keyword_4<-rel_query_df[22:26,1]


trends = gtrends (keyword, gprop = channel, geo=("CN"), time) 
write.table(trends$related_queries, "path.../16Mar-31Mar/CN_RelatedQueries.csv", row.names=FALSE)


trends1 = gtrends (keyword_1, gprop = channel, geo=("CN"), time) 
head (trends1$related_queries)
write.table(trends1$related_queries, "path.../16Mar-31Mar/CN_RelatedQueries.csv", row.names=FALSE, col.name=FALSE,append = TRUE)


trends2 = gtrends (keyword_2, gprop = channel, geo=("CN"), time) #query google trends per le ricerche google
head (trends2$related_queries)
write.table(trends2$related_queries, "path.../16Mar-31Mar/CN_RelatedQueries.csv", row.names=FALSE,col.name=FALSE, append = TRUE)


trends3 = gtrends (keyword_3, gprop = channel, geo=("CN"), time) #query google trends per le ricerche google
head (trends3$related_queries)
write.table(trends3$related_queries, "path.../16Mar-31Mar/CN_RelatedQueries.csv", row.names=FALSE,col.name=FALSE, append = TRUE)


trends4 = gtrends (keyword_4, gprop = channel, geo=("CN"), time) #query google trends per le ricerche google
head (trends4$related_queries)
write.table(trends4$related_queries, "path.../16Mar-31Mar/CN_RelatedQueries.csv", row.names=FALSE, col.name=FALSE, append = TRUE)

