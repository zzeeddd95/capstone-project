library(SnowballC)
library(textclean)
library(syuzhet)
library(rtweet)
library(devtools)
library(tibble)
library(ROAuth)
library(purrr)
library(stringr)
library(dplyr)
library(twitteR)
library(RCurl)
library(XML)
library(tm.plugin.sentiment)
library(tm)
library(wordcloud2)
citation('XML')
citation('SnowballC')
citation('textclean')
citation('syuzhet')
citation('rtweet')
citation('devtools')
citation('tibble')
citation('ROAuth')
citation('purrr')
citation('stringr')
citation('dplyr')
citation('twitteR')
citation('RCurl')
citation('tm.plugin.sentiment')
citation('tm')



#connecting to twitter

consumer_key <- "jHHSo1xVw0mhp1cyFTac86nK3"
consumer_secret <- "sEMM8ed0DVd8j9DaKnta8UkzEaMLdzwoYAet8mf3MWiitVewQp"
access_token <- "244645993-QrRcQWJXOb5SGBI8W6jxIL0uYMk686TIIGKn9a7Q"
access_secret <- "mZ1aa2TsvWDqfBVMU2GUEHUiglTjkmQQ7KUimGFGQ0ilP"
setup_twitter_oauth(consumer_key, consumer_secret, access_token, access_secret)




#Obtaining the first set of tweets
peter <- userTimeline("PeterLBrandt", n=1000)
josephburns <- userTimeline("SJosephBurns", n=1000)
elerian <- userTimeline("elerianm", n = 1000)
ibd <- userTimeline("IBDinvestors", n = 1000)
bespoke <- userTimeline("bespokeinvest", n = 1000)
marketwatch <- userTimeline("MarketWatch", n = 1000)
appletweet <- searchTwitter("$AAPL", n=1000, lang = 'en')
msfttweet <- searchTwitter("$MSFT", n = 1000,lang = 'en')
amazontweet <- searchTwitter("$AMZN", n=1000, lang = 'en')
googletweet <- searchTwitter("$GOOG", n = 1000, lang = 'en')
nastweet <- searchTwitter("$NASDAQ", n = 1000, lang = 'en')
#Transforming to dataframe
tweets1 <- tbl_df(map_df(c(peter, josephburns, elerian, ibd, bespoke, marketwatch, appletweet, msfttweet, googletweet, nastweet), as.data.frame))
#Saving into CSV
write.csv(tweets1, file = "tweets1.csv", row.names = FALSE)

#obtaining the second set of tweets

appleonly <- searchTwitter("#AAPL", n = 1000, lang = 'en')
applemoney <- searchTwitter("#$AAPL", n = 1000, lang = 'en')
msftonly <- searchTwitter("#microsoft", n = 1000, lang = 'en')
msftmoney <- searchTwitter("#$msft", n = 1000, lang = 'en')
amazononly <- searchTwitter("#amazon", n = 1000, lang = 'en')
amazonmoney <- searchTwitter("#$amzn,", n = 1000, lang = 'en')
googleonly <- searchTwitter("#google", n = 1000, lang = 'en')
googlemoney <- searchTwitter("#$goog", n = 1000, lang = 'en')
nasdaqonly <- searchTwitter("#nasdaq", n = 1000, lang = 'en')
nasdaqmoney <- searchTwitter("#$ndaq", n = 1000, lang = 'en')
#Transforming to dataframe
tweets2 <- tbl_df(map_df(c(appleonly, applemoney, msftonly, msftmoney, amazononly, amazonmoney, googleonly, googlemoney, nasdaqonly, nasdaqmoney), as.data.frame))
#Saving into CSV
write.csv(tweets2, file = "tweets2.csv", row.names = FALSE)


#setworking directory and read in data

setwd("C:/Users/ziyad/Desktop/Data Analytics capstone")
tweets1 <- read.csv("tweets1.csv")
tweets2 <- read.csv("tweets2.csv")

#twitter data cleanup for dataset1

twittercorpus1 <- Corpus(VectorSource(tweets1$text))
inspect(twittercorpus1[1:10])

twittercorpus1 <- tm_map(twittercorpus1, content_transformer(tolower))
twittercorpus1 <- tm_map(twittercorpus1, removeWords, stopwords("en"))
twittercorpus1 <- tm_map(twittercorpus1, removeNumbers)
twittercorpus1 <- tm_map(twittercorpus1, removePunctuation)
removeURLhttp1 <- function(x) gsub ("http[[:alnum:]]*", "", x)
twittercorpus1 <- tm_map(twittercorpus1, content_transformer(removeURLhttp1))
removeURLedua1 <- function(x) gsub ("edua[[:alnum:]]*", "", x)
twittercorpus1 <- tm_map(twittercorpus1, content_transformer(removeURLedua1))
removeNonAscii <- function(x) textclean::replace_non_ascii(x)
twittercorpus1 <- tm_map (twittercorpus1,content_transformer(removeNonAscii))
twittercorpus1 <- tm_map(twittercorpus1, stripWhitespace)
twittercorpus1 <- tm_map(twittercorpus1, removeWords, c('aapl','goog','msft','amzn','appl','apple','googl','microsoft','nasdaq','microsoft','amazon','gnusbrand','schwarzenegg', 'lizclaman','daverubin','qqq','tsla','spi','superherokindergarten','nonsen','plain','play'))
inspect(twittercorpus1[1:10])


#term document matrix for corpus 1 

twittercorpus1 <- tm_map(twittercorpus1, stemDocument)
dtm1 <- TermDocumentMatrix(twittercorpus1)
dtm1
termmatrix1 <- as.matrix(dtm1)
termmatrix1[1:10, 1:20]

#Bar plot

ex1 <- rowSums(termmatrix1)
ex1 <- subset(ex1, ex1>=25)
ex1
barplot(ex1, las = 2, col = rainbow(100))

#Creating the wordcloud

cloud1 <- sort(rowSums(termmatrix1), decreasing = TRUE)
cloud1 <- data.frame(names(cloud1), cloud1)
colnames(cloud1) <- c('word', 'freq')
wordcloud2(cloud1, size = 0.5, shape = 'circle', rotateRatio = 0.5, minSize = 1)


#twitter data cleanup for dataset2

twittercorpus2 <- Corpus(VectorSource(tweets2$text))
inspect(twittercorpus2[1:10])

twittercorpus2 <- tm_map(twittercorpus2, content_transformer(tolower))
twittercorpus2 <- tm_map(twittercorpus2, removeWords, stopwords("en"))
twittercorpus2 <- tm_map(twittercorpus2, removeNumbers)
twittercorpus2 <- tm_map(twittercorpus2, removePunctuation)
removeURLhttp2 <- function(x) gsub ("http[[:alnum:]]*", "", x)
twittercorpus2 <- tm_map(twittercorpus2, content_transformer(removeURLhttp2))
removeURLedua2 <- function(x) gsub ("edua[[:alnum:]]*", "", x)
twittercorpus2 <- tm_map(twittercorpus2, content_transformer(removeURLedua2))
removeNonAscii2 <- function(x) textclean::replace_non_ascii(x)
twittercorpus2 <- tm_map (twittercorpus2,content_transformer(removeNonAscii2))
twittercorpus2 <- tm_map(twittercorpus2, removeWords, c("amp","ufef","ufeff","ufeft","uufefuufefuufef","uufef","s",'aapl','goog','msft','amzn','appl','apple','googl','microsoft','nasdaq','microsoft','amazon','qqq','spi','...','follow','stock','adrian','friend','trade','eth','give','one','one','retweet','pip'))
twittercorpus2 <- tm_map(twittercorpus2, stripWhitespace)
inspect(twittercorpus2[1:10])


#term document matrix 

twittercorpus2 <- tm_map(twittercorpus2, stemDocument)
dtm2 <- TermDocumentMatrix(twittercorpus2)
dtm2
termmatrix2 <- as.matrix(dtm2)
termmatrix2[1:10, 1:20]

#unigran, how to use 2 adjacent words 
#(bigram) how to optmizie term frequency (tfidf) review literature. check package formula for specific function. how to compuyte tf-idf.

#Bar plot

ex2 <- rowSums(termmatrix2)
ex2 <- subset(ex2, ex2>=25)
ex2
barplot(ex2, las = 2, col = rainbow(100))

#Creating the wordcloud

cloud2 <- sort(rowSums(termmatrix2), decreasing = TRUE)
cloud2 <- data.frame(names(cloud2), cloud2)
colnames(cloud2) <- c('word', 'freq')
wordcloud2(cloud2, size = 1, shape = 'circle', rotateRatio = 0.5, minSize = 1)


#sentiment analysis

emotions1 <- get_nrc_sentiment(twittercorpus1$content)
barplot(colSums(emotions1),cex.names = .7, col = rainbow(10), main = "sentiment scores for tweets1")


emotions2 <- get_nrc_sentiment(twittercorpus2$content)
barplot(colSums(emotions2),cex.names = .7, col = rainbow(10), main = "sentiment scores for tweets2")

#use as an indeppendant variabvle, reduce bias by using a dictionary to compare. highlight specific step when using dictionary.

#next step to start looking at data. when mining tweets, focus on non textual analysis.