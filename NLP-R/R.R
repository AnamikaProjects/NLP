#Name- Anamika Singh
#Major- Business Analytics
#Student ID- 109450027
#Date- 12/05/2020

# Install packages
install.packages("dplyr")
install.packages("rtweet")
install.packages("httpuv")
install.packages("tidytext")
install.packages("tm")
install.packages("tidyr")
install.packages("ggthemes")
install.packages("ggplot2")
install.packages('textdata')
install.packages("wordcloud")
install.packages("RColorBrewer")

library("dplyr")
library("rtweet")
library("httpuv")
library("tidytext")
library("tm")
library("tidyr")
library("ggthemes")
library("ggplot2")
library('textdata')
library("wordcloud")
library("RColorBrewer")


# Create twitter API Key to download data from twitter
app_name <- "TextAnalyticsAnamika"
api_key <- "I3F8O5unO1Ze2rBcGzqtR1kSf"
api_key_secret <- "PVtuG2JRGsckJyVNZIVTEUJjBpaL5VAx5LzZgyjBLgh6gsFG1i"
Access_token <-"1330252557915213827-kMOWzVljfst0Gcvzsnr6fTlXsTF7vN"
Access_token_secret <-"AivCF5QvQj2kjaCDdCFCZzxul6ZE4A8hzVwJOtApZZ4Ay"

# Create token to download Twitter data
create_token(app=app_name, consumer_key=api_key, 
             consumer_secret = api_key_secret,access_token=Access_token,
             access_secret= Access_token_secret, set_renv = TRUE)

# Tags related to Samsung products and Apple products
tag=c("#Samsung","#samsung","#samsunggalaxy", "#galaxynote", "#galaxy",
      "#samsunggalaxys","#samsungs","#samsunggalaxynote","#Apple","#apple",
      "#ipad", "#iphone","#appl","#applewatch","#airpods")

# Search for about 35000 tweets using hashtag from Twitter
search_keyword <- paste(tag, collapse = " OR ")
tweets.data <- search_tweets(search_keyword, n=35000,lang="en",type="mixed",
                             retryonratelimit = TRUE, include_rts = FALSE)

dim(tweets.data)
glimpse(tweets.data)
tweets.data$text
class(tweets.data)
head(tweets.data)


# Visualize frequency of tweets over time
ts_plot(tweets.data,"1 hours") + theme_minimal() + 
theme(plot.title=ggplot2::element_text(face="bold")) +
labs(x=NULL, y=NULL, title="Frequency of tweets",
subtitle='Tweet status counts')


#------------------------------------------------------------------------
################## Exporting the data into csv file ####################
#------------------------------------------------------------------------

# Export the data into a csv file
tweets <- apply(tweets.data,2,as.character)
write.csv(tweets,"C:/Users/anami/Desktop/MS BA/AFall 2020/Text Analytics/Assignment/HW4/tweets.csv", row.names = FALSE)

# Read the csv file data
getwd()
setwd("C:/Users/anami/Desktop/MS BA/AFall 2020/Text Analytics/Assignment/HW4")
tweets<- read.csv("tweets.csv")
class(tweets)


#----------------------------------------------------------------------
######################## Initial Data cleaning ########################
#----------------------------------------------------------------------

install.packages("stringr")
library("stringr")  # To use str_replace_all

# To remove Hashtags,Twitter handles,URL,Unicode,and ampersand from the text
tweets$text <- str_replace_all(tweets$text,"#[a-z,A-Z]*|@[a-z,A-Z]*|http.*|\\s*<U\\+\\w+>|&amp","")
tweets$text[1:5]

# Rename country
tweets$country <- gsub('United States','USA', tweets$country)
tweets$country <- gsub('United Kingdom','UK', tweets$country)
tweets$country <- gsub("People's Republic of China","China", tweets$country)
unique(tweets$country)


# Subset tweets related to Samsung products
SamsungTweets <-subset(tweets, grepl(c('samsung|samsunggalaxy|galaxynote|galaxy|samsunggalaxys|samsungs|samsunggalaxynote'), tweets$hashtags, ignore.case=TRUE ))
# To remove duplicated rows
SamsungTweets<- SamsungTweets %>% select(text,hashtags,country,geo_coords,
                                         coords_coords,bbox_coords) %>%
                                         distinct(text, .keep_all = TRUE)
SamsungText<-SamsungTweets$text


# Subset tweets related to apple products
AppleTweets <-subset(tweets, grepl(c('apple|ipad|iphone|appl|applewatch|airpods'), tweets$hashtags, ignore.case=TRUE ))
# To remove duplicated rows
AppleTweets <-AppleTweets %>% select(text,hashtags,country,geo_coords,
                                     coords_coords,bbox_coords) %>%
                                     distinct(text, .keep_all = TRUE)
AppleText<-AppleTweets$text


#-------------------------------------------------------------------------
########################## Building corpus ###############################
#-------------------------------------------------------------------------

# Using tweets on Samsung products

# Converting tweets text into ASCII and creating Samsung corpus
Sam_tweets<- iconv(SamsungText, "latin1", "ASCII", sub="")
Sam_tweets <- data.frame(doc_id=seq(1:nrow(SamsungTweets)), text=SamsungText)
samCorpus <- VCorpus(DataframeSource(Sam_tweets))
inspect(samCorpus)
samCorpus[[1]]$content
samCorpus[[1]]$meta
class(samCorpus)


# Using tweets on Apple products

# Converting tweets text into ASCII and creating Apple corpus
App_tweets<- iconv(AppleText, "latin1", "ASCII", sub="")
App_tweets <- data.frame(doc_id=seq(1:nrow(AppleTweets)), text=AppleText)
appCorpus <- VCorpus(DataframeSource(App_tweets))
inspect(appCorpus)
appCorpus[[1]]$content
appCorpus[[1]]$meta
class(appCorpus)


# Pre-processing to clean data
clean.corpus <- function(corpus){
  corpus <- tm_map(corpus, content_transformer(tolower))
  corpus <- tm_map(corpus, removeWords, stopwords("en"))
  corpus <- tm_map(corpus, removePunctuation)
  corpus <- tm_map(corpus, stripWhitespace)
  corpus <- tm_map(corpus, removeNumbers)
  corpus<-tm_map(corpus, stemDocument, language="en")
}

# Put data into above function to clean the data
samsung_data <- clean.corpus(samCorpus)
inspect(samsung_data[1])
samsung_data[[1]]$content
# Put data into above function to clean the data
apple_data <- clean.corpus(appCorpus)
inspect(apple_data[1])
apple_data[[1]]$content

# Creating function to remove names of the products(Samsung and galaxy)
RemoveProductNames <- function(x) {
  x <- tm_map(x, removeWords, c("samsung", "galaxy","galaxi","iphone","apple","appl","iphon"))
  return(x)
}

# Creating a a function to prepare Term Document Matrix
CreateTermsMatrix <- function(x) {
  x <- TermDocumentMatrix(x)
  x <- as.matrix(x)
  y <- rowSums(x)
  y <- sort(y, decreasing=TRUE)
  y <- data.frame(word = names(y),freq=y)
  return(y)
}

# Put data into above function to create TDM and dataframe
samsung<-RemoveProductNames(samsung_data)
DFSamsung <- CreateTermsMatrix(samsung)
# Put data into above function to create TDM and dataframe
apple<-RemoveProductNames(apple_data)
DFapple <- CreateTermsMatrix(apple)


# Bar plot of Top 20 words used on twitter for Samsung products
df1<-DFSamsung[1:20,] %>%
  ggplot(aes(x=(reorder(word, freq)),y=freq)) +
  geom_bar(stat='identity', fill="red") + coord_flip() + 
  theme_gdocs() + geom_text(aes(label=freq), colour='black',
  hjust=1.25, size=4.0) + ggtitle("Samsung products tweets") + 
  labs(x="Top 20 Words", y="Frequency of Each Word")


# Word Cloud Samsung products
wordcloud(DFSamsung$word, DFSamsung$freq, max.words = 100, 
          scale=c(2.5,.4), random.color = TRUE, colors=brewer.pal(8,"Dark2"))


# Bar plot of Top 20 words used on twitter for Apple products
df2<-DFapple[1:20,] %>%
  ggplot(aes(x=(reorder(word, freq)),y=freq)) +
  geom_bar(stat='identity', fill="green") + coord_flip() + 
  theme_gdocs() + geom_text(aes(label=freq), colour='black',
  hjust=1.25, size=4.0) + ggtitle("Apple products tweets") + 
  labs(x="Top 20 Words", y="Frequency of Each Word")

# Word Cloud Apple products
wordcloud(DFapple$word, DFapple$freq, max.words = 100, 
          scale=c(2.5,.4), random.color = TRUE, colors=brewer.pal(8,"Dark2"))

# Present all thE bar plots side by side
require(gridExtra)
grid.arrange(df1,df2,ncol=2)


#--------------------------------------------------------------------------
##################### Sentiment Analysis ##################################
#--------------------------------------------------------------------------

# Create a function for document term matrix and retain only non-sparse values
Createtidy<-function(x){
  x= DocumentTermMatrix(x)
  x= tidy(x)   
  return(x)
}

# Pass the Samsung data through above function
sam.tidy <- Createtidy(samsung)
sam.tidy
# Set new column names to sam.tidy data table
colnames(sam.tidy) <- c('line_number', 'word', 'count')

# Pass the Apple data through above finction
app.tidy<- Createtidy(apple)
app.tidy
# Set new column names to app.tidy data table
colnames(app.tidy) <- c('line_number', 'word', 'count')


#-------------------------------------------------------------------------
########### Calling Bing dictionary for sentiment analysis ###############
#-------------------------------------------------------------------------

# Call bing dictionary
bing.sentiment = get_sentiments("bing")
# Inner join the bing words on sam.tidy table
sam.sentiment <- sam.tidy %>% inner_join(bing.sentiment)
sam.sentiment
# Inner join the bing words on app.tidy table
app.sentiment <- app.tidy %>% inner_join(bing.sentiment)
app.sentiment

# Positive words used for Samsung products
sam.sentiment.pos <- sam.sentiment %>%  filter(sentiment=='positive')%>% 
                     select(word) %>% 
                     group_by(word) %>%
                     summarise(n=length(word),.groups = 'drop') %>%
                     arrange(-n)
sam.sentiment.pos

# Negative words used for Samsung products
sam.sentiment.neg <- sam.sentiment %>%  filter(sentiment=='negative')%>% 
                     select(word) %>% 
                     group_by(word) %>%
                     summarise(n=length(word),.groups = 'drop') %>%
                     arrange(-n)
sam.sentiment.neg

# Positive words used for Apple products
app.sentiment.pos <- app.sentiment %>%  filter(sentiment=='positive')%>% 
                     select(word) %>% 
                     group_by(word) %>%
                     summarise(n=length(word),.groups = 'drop') %>%
                     arrange(-n)
app.sentiment.pos

# Negative words used for Apple products
app.sentiment.neg <- app.sentiment %>%  filter(sentiment=='negative')%>% 
                     select(word) %>% 
                     group_by(word) %>%
                     summarise(n=length(word),.groups = 'drop') %>%
                     arrange(-n)
app.sentiment.neg

# Bar plot with top 20 most used positive and negative words for 
# Samsung products
bar1<-sam.sentiment.pos[1:20,] %>%
ggplot(aes(x=(reorder(word, n)),y=n)) +
geom_bar(stat='identity', fill="blue") + coord_flip() + 
theme_gdocs() + geom_text(aes(label=n), colour='black',
hjust=1.25, size=4.0) + ggtitle("Samsung products positive words") + 
labs(x="Top 20 Words", y="Frequency of Each Word")


bar2<-sam.sentiment.neg[1:20,] %>%
ggplot(aes(x=(reorder(word, n)),y=n)) +
geom_bar(stat='identity', fill="red") + coord_flip() + 
theme_gdocs() + geom_text(aes(label=n), colour='black',
hjust=1.25, size=4.0) + ggtitle("Samsung products negative words") + 
labs(x="Top 20 Words", y="Frequency of Each Word")


# Bar plot with top 20 most used positive and negative words for 
# Apple products
bar3<-app.sentiment.pos[1:20,] %>%
ggplot(aes(x=(reorder(word, n)),y=n)) +
geom_bar(stat='identity', fill="blue") + coord_flip() + 
theme_gdocs() + geom_text(aes(label=n), colour='black',
hjust=1.25, size=4.0) + ggtitle("Apple products positive words") + 
labs(x="Top 20 Words", y="Frequency of Each Word")


bar4<-app.sentiment.neg[1:20,] %>%
ggplot(aes(x=(reorder(word, n)),y=n)) +
geom_bar(stat='identity', fill="red") + coord_flip() + 
theme_gdocs() + geom_text(aes(label=n), colour='black',
hjust=1.25, size=4.0) + ggtitle("Apple products negative words") + 
labs(x="Top 20 Words", y="Frequency of Each Word")

# Present all the four barplots side by side
require(gridExtra)
grid.arrange(bar1,bar2,bar3,bar4 , ncol=2)


# Word cloud of positive and negative words for Samsung products

install.packages("wordcloud2")
library("wordcloud2")

# Positive
wordcloud2(sam.sentiment.pos, size=0.5, shape='oval',
           rotateRatio = 0.4,minSize = 1)

# Negative
wordcloud2(sam.sentiment.neg, size=0.5, shape='oval',
           rotateRatio = 0.4,minSize = 1)

# Word cloud of positive and negative words for Apple products
# Positive
wordcloud2(app.sentiment.pos, size=0.5, shape='circle',
           rotateRatio = 0.4,minSize = 1)

# Negative
wordcloud2(app.sentiment.neg, size=0.5, shape='circle',
           rotateRatio = 0.4,minSize = 1)


#--------------------------------------------------------------------------
############ Calling nrc dictionary for sentiment analysis ###############
#--------------------------------------------------------------------------

# Call nrc dictionary
nrc.sentiment <- tidytext::get_sentiments("nrc")

# Analysis on Samsung products
# Inner join nrc sentiments on sam.tidy table
sam.nrc <- sam.tidy %>% inner_join(nrc.sentiment)
sam.nrc

# Plot to show all the nrc sentiments
Samsung.plot<-sam.nrc %>% count(sentiment) %>%
ggplot(aes(x=sentiment, y=n, fill=sentiment)) +
geom_bar(stat="identity") + coord_polar() +
theme(legend.position = "none", axis.text.x = element_blank()) +
geom_text(aes(label=sentiment, y=2000)) +
labs(x="", y="", title="Samsung Products")

# Analysis on Apple products
# Inner join nrc sentiments on app.tidy table
app.nrc <- app.tidy %>% inner_join(nrc.sentiment)
app.nrc

# Plot to show all the nrc sentiments
apple.plot<-app.nrc %>% count(sentiment) %>%
  ggplot(aes(x=sentiment, y=n, fill=sentiment)) +
  geom_bar(stat="identity") + coord_polar() +
  theme(legend.position = "none", axis.text.x = element_blank()) +
  geom_text(aes(label=sentiment, y=7500)) +
  labs(x="", y="", title="Apple Products")

# Side by side representation
require(gridExtra)
grid.arrange(Samsung.plot, apple.plot, ncol=2)


#------------------------------------------------------------------------
########### Calling AFINN dictionary for sentiment analysis #############
#------------------------------------------------------------------------

# Call afinn dictionary to calculate sentiment score
afinn.sentiment<-get_sentiments("afinn")
unique(afinn.sentiment$value)

# Sentiment score for Samsung products
# Inner join afinn sentiments on sam.tidy table
sam.afinn <- sam.tidy %>% inner_join(afinn.sentiment)
sam.afinn$line_number<-as.numeric(sam.afinn$line_number)
sam.afinn
# Calculate the score for each tweets
Sam.score <- sam.afinn %>% select(line_number,value) %>%
             group_by(index= line_number) %>%
             summarise(sentiment.score=sum(value),.groups = 'drop')
Sam.score

# Bar plot for 300 line of scores
score.plot1<-Sam.score[1:300,] %>% ggplot(aes(x=index, y=sentiment.score)) + 
              geom_bar(stat="identity", fill="red", position = "identity", width=5)+labs(x="Index", y="Score", title="Samsung score plot")+
              theme_gdocs()
# Smoothing the plot to get better understanding
ggplot(Sam.score[1:300,], aes(index, sentiment.score)) + 
stat_smooth() + theme_gdocs()


# Sentiment score for Apple products
# Inner join afinn sentiments on app.tidy table
app.afinn <- app.tidy %>% inner_join(afinn.sentiment)
app.afinn$line_number<-as.numeric(app.afinn$line_number)
app.afinn
# Calculate the score for each tweet on apple products
app.score <- app.afinn %>% select(line_number,value) %>%
             group_by(index= line_number) %>%
             summarise(sentiment.score=sum(value),.groups = 'drop')
app.score

# Barplot for 300 line of scores
score.plot2<-app.score[1:300,] %>% ggplot(aes(x=index, y=sentiment.score)) + 
  geom_bar(stat="identity", fill="blue", position = "identity", width=5) + labs(x="Index", y="Score", title="Apple score plot")+
  theme_gdocs()
# Smoothing the plot to get better understanding
ggplot(app.score[1:300,], aes(index, sentiment.score)) + 
  stat_smooth() + theme_gdocs()

# Side by side representation
require(gridExtra)
grid.arrange(score.plot1,score.plot2, ncol=2)


#--------------------------------------------------------------------------
############################# Bigrams ###################################
#--------------------------------------------------------------------------

# Function to plot the top 25 frequent bigrams
plotBigrams <- function(tibble, topN=25, title="", color="#FF1493"){
  x <- tibble %>% select(word) %>%
    unnest_tokens(bigram, word, token = "ngrams", n = 2)
  y <- x %>% count(bigram, sort = TRUE) %>% top_n(topN, wt=n) %>%
    ggplot(aes(x=reorder(bigram, n), y=n)) +
    geom_bar(stat='identity', fill=color) + coord_flip() +
    theme(legend.position="none") + labs(x="", title=title)
}

# Plot the bigrams for samsung data and apple data
a1 <- plotBigrams(sam.tidy, title="Samsung", color="red")
a2 <- plotBigrams(app.tidy, title="Apple", color="blue")

# Present the barplot side by side
require(gridExtra)
grid.arrange(a1, a2, nrow=1)


#-------------------------------------------------------------------------
####################### Word network graph ###############################
#-------------------------------------------------------------------------

install.packages('igraph')
library('igraph')

# Find the word network with Samsung
grepTweet <- grep("samsung", Sam_tweets$text, ignore.case = TRUE)
grepTweet[1:2]

# Take only two rows to get clear visualization
relatedSam.tweets <- Sam_tweets[grepTweet[1:2],]
Sam_tweets$text[1]
class(relatedSam.tweets)

# Create a corpus
relatedSam.corpus <- VCorpus(DataframeSource(relatedSam.tweets))
relatedSam.corpus <- clean.corpus(relatedSam.corpus)
class(relatedSam.corpus)
relatedSam.corpus[[1]]$content 
relatedSam.corpus[[1]]$meta 
inspect(relatedSam.corpus)

# Create a TDM for the corpus
relatedSam.tdm <- TermDocumentMatrix(relatedSam.corpus, control=list(weighting=weightTf))
relatedSam.tdm
inspect(relatedSam.tdm)

# Prepare adjacent matrix
matrix <- as.matrix(relatedSam.tdm)
dim(matrix)
matrix.adjacency <- matrix%*%t(matrix)
matrix.adjacency[1:5, 1:5]
dim(matrix.adjacency)
matrix.adjacency <- graph.adjacency(matrix.adjacency, weighted=TRUE, mode="undirected", diag=T)
matrix.adjacency
matrix.adjacency <- simplify(matrix.adjacency)
matrix.adjacency
 
# Plot the word network graph
plot.igraph(matrix.adjacency, vertex.shape="none",
            vertex.label.color="darkred",vertext.label.font=0.6, 
            vertext.label.cex=6,label.size=0.5, edge.color="gray70")
title(main="Samsung Word Network")


#-------------------------------------------------------------------------
########################## Dendrogram ###################################
#-------------------------------------------------------------------------

install.packages("ape")
library("ape")

# Dendrogram for Samsung products
# Create TDM for samsung data and remove sparse terms
sam.tdm <- TermDocumentMatrix(samsung_data, control=list(weighting=weightTf))
sam.dendrogram <- removeSparseTerms(sam.tdm, sparse = 0.975)

# Use euclidean method for calculation
dd <- dist(sam.dendrogram, method="euclidian")
hc <- hclust(dd, method = "complete")
colors = c("red", "blue", "green", "black")
clus4 = cutree(hc, 4)

# Plot the fan shape dendrogram for samsung data
plot(as.phylo(hc), type = "fan", tip.color = colors[clus4],
     label.offset = 1, cex = 0.6, main="Samsung Dendrogram")


# Dendrogram for Apple products
# Create TDM for apple data and remove sparse terms
App.tdm <- TermDocumentMatrix(apple_data, control=list(weighting=weightTf))
App.dendrogram <- removeSparseTerms(App.tdm, sparse = 0.975)

# Use euclidean method for calculation
dd1 <- dist(App.dendrogram, method="euclidian")
hc1 <- hclust(dd1, method = "complete")
hcd <- as.dendrogram(hc1)
nodePar <- list(lab.cex = 0.6, pch = c(NA, 19), 
                cex = 0.7, col = "blue")

# Plot the dendrogram for apple data
plot(hcd,  xlab = "Height", nodePar = nodePar, horiz = TRUE,
     edgePar = list(col = 2:3, lwd = 2:1), main="Apple Dendrogram")


#--------------------------------------------------------------------------
########################## Comparison Cloud ##############################
#--------------------------------------------------------------------------

# Paste all the samsung tweet and all the apple tweets
tweets.sam <- paste(sam.sentiment$word , collapse=' ')
tweets.app <- paste(app.sentiment$word, collapse=' ')

# Combine the tweets
tweets.all <- c(tweets.sam, tweets.app)
tweets.all[1]
tweets.all[2]

# Convert into corpus
tweets.corpus <- VCorpus(VectorSource(tweets.all))
tweets.terms <- TermDocumentMatrix(tweets.corpus)
# Create a matrix
tweets.matrix <- as.matrix(tweets.terms)
dim(tweets.matrix)
# Name the columns
colnames(tweets.matrix) <- c("Samsung", "Apple")

# Common word clouds between Samsung and Apple Tweets
commonality.cloud(tweets.matrix, max.words=200, random.order=FALSE, colors=brewer.pal(6, "Accent"))

# Comparison word clouds between Samsung and Apple Tweets
comparison.cloud(tweets.matrix, max.words=150, 
                 colors=brewer.pal(3, "Dark2"),
                 random.order=FALSE, title.size=2)


#----------------------------------------------------------------------------
########################### Common words ################################
#----------------------------------------------------------------------------

install.packages("plotrix")
library("plotrix")

# Find the similar and the difference in words
Similar <- subset(tweets.matrix, tweets.matrix[,2]>0 & tweets.matrix[,1]>0)
difference <- abs(Similar[,1]-Similar[,2])
head(Similar[,1])
head(Similar[,2])
# combine the columns
common.words <- cbind(Similar, difference)
head(common.words)

# Arrange the common words in descending order and take top 20 in a dataframe
common.words <- common.words[order(common.words[,3], decreasing = T),]
head(common.words)
DF<- data.frame(x = common.words[1:20,1], y = common.words[1:20,2], labels=rownames(common.words[1:20,]))
# Plot the top 20 common words
pyramid.plot(DF$x, DF$y, labels=DF$labels, gap=50, 
             top.labels = c("Samsung", "Words", "Apple"),
             main="Common words", laxlab = NULL, raxlab = NULL, unit=NULL)


#---------------------------------------------------------------------------
####################### Geo-Map Visualization #############################
#---------------------------------------------------------------------------

install.packages("here")
install.packages("tidyverse")
install.packages("ggrepel")
install.packages("sp")
install.packages("socviz")
install.packages("rworldmap")
install.packages("maps")

library(here)
library(tidyverse)
library(ggrepel)
library(sp)
library(socviz)
library(rworldmap)
library(maps)

# Geo-code tweets posted by those users who permitted to track their locations
# tweets.data is the data downloaded from twitter
geocode <- lat_lng(tweets.data)
glimpse(geocode)
length(geocode)
# Tweets geo-codes on the world map.
par(mar=c(0,0,0,0)) # Set margin in the order of: bottom, left, top, and right
maps::map("world", lwd=0.25)
with(geocode, points(lng, lat, pch=20, cex=.75, col=rgb(0, .3, .7, .75)))


# Base World map
worldmap <- map_data("world")
dim(worldmap)
map<-ggplot(data=worldmap, 
       mapping=aes(x=long, y=lat, group=group))+ geom_polygon(fill="white", color="gray30")
map


# Map data based on country
ggplot(data=worldmap,mapping = aes(x=long, y=lat, group=group, 
      fill=region))+ geom_polygon(color="gray", size=0.2) + guides(fill=FALSE)


# Join Samsung data with world map and group by country
sam.map<-SamsungTweets %>% group_by(country) %>% summarise(n=length(country),.groups = 'drop')
# Remove na 
sam.map<-sam.map[!is.na(sam.map$country), ]
sam.map$region<-sam.map$country
world.tweets <- left_join(worldmap,sam.map, by="region")
dim(sam.map)
dim(world.tweets)

# Map based on tweets location for Samsung products
ggplot(data=world.tweets,mapping = aes(x=long, y=lat, group=group, 
       fill=n)) + geom_polygon(color="gray", size=0.1)+ 
       labs(title = "Samsung Tweets by Country")+ labs(fill = "Tweets") + 
       scale_fill_gradient(low = "white", high = "#CB454A") 


# Join apple data with world map and group by country
app.map<-AppleTweets %>% group_by(country) %>% summarise(n1=length(country),.groups = 'drop')
# Remove na
app.map<-app.map[!is.na(app.map$country), ]
app.map$region<-app.map$country
world.tweets1 <- left_join(worldmap,app.map, by="region")
dim(app.map)
dim(world.tweets1)

# Map based on tweets location for Apple products
ggplot(data=world.tweets1,mapping = aes(x=long, y=lat, group=group, 
       fill=n1)) + geom_polygon(color="gray", size=0.2)+
       labs(title = "Apple Tweets by Country")+ labs(fill = "Tweets") + 
  scale_fill_gradient(low = "white", high = "#2E74C0") 


# Difference in number of tweets for Samsung products and Apple products 
# based on country
# Inner join Samsung and Apple data
map.all<-inner_join(sam.map,app.map)
map.all
# left join the world data
world.tweets.all <- map.all %>% select(region,n,n1) %>% mutate(diff=(n1-n))
world.tweets.all<-left_join(worldmap,world.tweets.all, by="region")

# Map based on difference in number of tweets by countries
ggplot(data = world.tweets.all,mapping = aes(x = long, y = lat, group = group, 
      fill = diff))+geom_polygon(color = "gray90", size = 0.1)+
      labs(title = "Difference in number of tweets")+ labs(fill = "Difference")+ 
      scale_fill_gradient2(low = "red", mid = scales::muted("purple"),
                           high = "blue", breaks = c(100, 200, 300,400)) 


