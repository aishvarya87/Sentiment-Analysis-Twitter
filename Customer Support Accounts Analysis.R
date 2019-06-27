
#TWITTER CONNECTION

  if (!require('devtools')) install.packages('devtools')
devtools::install_github('rstudio/leaflet')

install.packages("yaml")
library(yaml)

install.packages("twitteR")

library(twitteR)

#Using mt personal Twitter keys
api_key <- "KuUiiNah1zaZYtAz0S74k36Jq"
api_secret <- "7ArNyLhrnxXhj5t7BE6NvccWXuaq3pS94jUP9YRRnQU94w5yJH"
token <- "103206999-WX5ezdiQXtat59jwk0yDpqCHZoD8WPTINNoaGHG1"
token_secret <- "Ahguqu5RWk6wEGm5LlIKqUVpUTNsJMuC3zLQNv81j01st"

#Authenticate from Twitter
setup_twitter_oauth(api_key, api_secret, token, token_secret)


install.packages("leaflet") 
install.packages("maps") 
library(leaflet) 
library(maps)


#search Twitter
tweets <- searchTwitter("Samsung", n = 200, lang = "en")

# save to a data frame
tweets.df <-twListToDF(tweets)

#############################################################################################

#an example of a file extension of the folder in which you want to save the .csv file.
write.csv(tweets.df, "C:\\Users\\aishvarya.b\\Documents\\Work related documents\\Analytics\\samsung_tweets.csv") 


read.csv("C:\\Users\\aishvarya.b\\Documents\\Work related documents\\Analytics\\samsung_tweets.csv", stringsAsFactors = FALSE)

map <- leaflet(mymap) %>% addTiles()
##############################################################################################

tweets <- twitteR::searchTwitter("#Samsung",n=12,lang ="en",since = "2018-01-01")
strip_retweets(tweets)


df <- twListToDF(tweets)#extract the data frame save it locally

write.csv(tweets.df, "C:\\Users\\aishvarya.b\\Documents\\Work related documents\\Forecast Modelling\\Tableau FCST method\\tweets3.csv") 
df1 <- read.csv("C:\\Users\\aishvarya.b\\Documents\\Work related documents\\Forecast Modelling\\Tableau FCST method\\tweets3.csv", stringsAsFactors = FALSE)

library(dplyr)
dplyr::distinct(df1)

winner <-df1 %>% select(text,retweetCount,screenName,id )%>% filter(retweetCount == max(retweetCount))
View(winner)




#TEXT MINING

#install.packages("twitteR")
library(twitteR)

# install.packages("NLP", lib="C:/Program Files/R/R-3.5.0/library")
# install.packages("syuzhet", lib="C:/Program Files/R/R-3.5.0/library")
# install.packages("tm", lib="C:/Program Files/R/R-3.5.0/library")

library("NLP")
library("syuzhet")
library("tm")
library("SnowballC")
library("stringi")
library("topicmodels")


#Using my personal Twitter keys
api_key <- "KuUiiNah1zaZYtAz0S74k36Jq"
api_secret <- "7ArNyLhrnxXhj5t7BE6NvccWXuaq3pS94jUP9YRRnQU94w5yJH"
token <- "103206999-WX5ezdiQXtat59jwk0yDpqCHZoD8WPTINNoaGHG1"
token_secret <- "Ahguqu5RWk6wEGm5LlIKqUVpUTNsJMuC3zLQNv81j01st"

#Authenticate from Twitter
setup_twitter_oauth(api_key, api_secret, token, token_secret)

##########################################################################################################################

tweets_a <- searchTwitter("#Apple OR #iPhoneX", n=1000,lang = "en", since="2018-01-01")
tweets_s <- searchTwitter("#Samsung OR #GalaxyS9", n=1000,lang = "en", since="2018-01-01")
tweets_g <- searchTwitter("#Google OR #Pixel", n=1000,lang = "en", since="2018-01-01")

apple_tweets.df <- twListToDF(tweets_a)
samsung_tweets.df <- twListToDF(tweets_s)
google_tweets.df <- twListToDF(tweets_g)

View(apple_tweets.df)
View(samsung_tweets.df)
View(google_tweets.df)

#####Convert to Text - Method 1 ###################################################

# build a corpus, and specify the source to be character vectors
apple_Corpus <- Corpus(VectorSource(apple_tweets.df$text))
samsung_Corpus <- Corpus(VectorSource(samsung_tweets.df$text))
google_Corpus <- Corpus(VectorSource(google_tweets.df$text))

apple_Corpus <- sapply(apple_Corpus, function(row) iconv(row, "latin1", "ASCII", sub=""))
apple_Corpus <- Corpus(VectorSource(apple_Corpus))

samsung_Corpus <- sapply(samsung_Corpus, function(row) iconv(row, "latin1", "ASCII", sub=""))
samsung_Corpus <- Corpus(VectorSource(samsung_Corpus))

google_Corpus <- sapply(google_Corpus, function(row) iconv(row, "latin1", "ASCII", sub=""))
google_Corpus <- Corpus(VectorSource(google_Corpus))

#convert to lower case
apple_Corpus <- tm_map(apple_Corpus, content_transformer(tolower))
samsung_Corpus <- tm_map(samsung_Corpus, content_transformer(tolower))
google_Corpus <- tm_map(google_Corpus, content_transformer(tolower))

# # remove punctuation
apple_Corpus <- tm_map(apple_Corpus, removePunctuation)
samsung_Corpus <- tm_map(samsung_Corpus, removePunctuation)
google_Corpus <- tm_map(google_Corpus, removePunctuation)
#
# remove numbers
apple_Corpus <- tm_map(apple_Corpus, removeNumbers)
samsung_Corpus <- tm_map(samsung_Corpus, removeNumbers)
google_Corpus <- tm_map(google_Corpus, removeNumbers)

# remove URLs
removeURL <- function(x) gsub("(ftp|http)(s?)://.*\\b", "", x)

# # ### myCorpus <- tm_map(myCorpus, removeURL, lazy=TRUE)
apple_Corpus <- tm_map(apple_Corpus, content_transformer(removeURL))
samsung_Corpus <- tm_map(samsung_Corpus, content_transformer(removeURL))
google_Corpus <- tm_map(google_Corpus, content_transformer(removeURL))
# #

# add extra stop words: 'available' and 'via'
myStopwords1 <- c(stopwords("english"), "available", "via", "rt")
myStopwords2<- c(stopwords("SMART"), "ae", "rt")

# remove stopwords from corpus
apple_Corpus <- tm_map(apple_Corpus, removeWords, myStopwords1)
samsung_Corpus <- tm_map(samsung_Corpus, removeWords, myStopwords1)
google_Corpus <- tm_map(google_Corpus, removeWords, myStopwords1)
#
apple_Corpus <- tm_map(apple_Corpus, removeWords, myStopwords2)
samsung_Corpus <- tm_map(samsung_Corpus, removeWords, myStopwords2)
google_Corpus <- tm_map(google_Corpus, removeWords, myStopwords2)
##

#???# keep a copy of corpus to use later as a dictionary for stem
# completion
apple_CorpusCopy <- apple_Corpus
samsung_CorpusCopy <- samsung_Corpus
google_CorpusCopy <- google_Corpus

# stem words
#apple_Corpus <- tm_map(apple_Corpus, stemDocument)
#samsung_Corpus <- tm_map(samsung_Corpus, stemDocument)
#google_Corpus <- tm_map(google_Corpus, stemDocument)


#################################################################################

########## Mining ###########################

# inspect the first 5 documents (tweets) inspect(myCorpus[1:5]) 
# The code below is used for to make text fit for paper width 
for (i in 1:5) {
  cat(paste("[[", i, "]] ", sep = ""))
  #writeLines(myCorpus[[i]])
  writeLines(as.character(apple_Corpus[[i]]))
}

for (i in 1:5) {
  cat(paste("[[", i, "]] ", sep = ""))
  #writeLines(myCorpus[[i]])
  writeLines(as.character(samsung_Corpus[[i]]))
}

for (i in 1:5) {
  cat(paste("[[", i, "]] ", sep = ""))
  #writeLines(myCorpus[[i]])
  writeLines(as.character(google_Corpus[[i]]))
}

#################### Stem completion ############################

# myCorpus <- tm_map(myCorpus, content_transformer(stemCompletion), dictionary = myCorpusCopy, lazy=TRUE)

#apple_Corpus <- tm_map(apple_Corpus, content_transformer(stemCompletion), dictionary = apple_CorpusCopy)
#samsung_Corpus <- tm_map(samsung_Corpus, content_transformer(stemCompletion), dictionary = samsung_CorpusCopy)
#google_Corpus <- tm_map(google_Corpus, content_transformer(stemCompletion), dictionary = google_CorpusCopy)

#################################################################################################

################################## count frequency of "mining" ##################################


apple_tdm <- TermDocumentMatrix(apple_Corpus, control = list(wordLengths = c(3, Inf)))
apple_tdm

samsung_tdm <- TermDocumentMatrix(samsung_Corpus, control = list(wordLengths = c(3, Inf)))
samsung_tdm

google_tdm <- TermDocumentMatrix(google_Corpus, control = list(wordLengths = c(3, Inf)))
google_tdm

## Freqency words and Association
apple_idx <- which(dimnames(apple_tdm)$Terms == "iPhoneX")
inspect(apple_tdm[apple_idx + (0:5), 10:20])

samsung_idx <- which(dimnames(samsung_tdm)$Terms == "Galaxy")
inspect(samsung_tdm[samsung_idx + (0:5), 10:20])

google_idx <- which(dimnames(google_tdm)$Terms == "Pixel")
inspect(google_tdm[google_idx + (0:5), 10:20])

#inspect frequent words
(freq.terms <- findFreqTerms(apple_tdm, lowfreq=15))
(freq.terms <- findFreqTerms(samsung_tdm, lowfreq=15))
(freq.terms <- findFreqTerms(google_tdm, lowfreq=15))

term.freq <- rowSums(as.matrix(apple_tdm))
term.freq <- subset(term.freq, term.freq >=5)
apple_df <- data.frame(term = names(term.freq), freq = term.freq)

term.freq <- rowSums(as.matrix(samsung_tdm))
term.freq <- subset(term.freq, term.freq >=5)
samsung_df <- data.frame(term = names(term.freq), freq = term.freq)

term.freq <- rowSums(as.matrix(google_tdm))
term.freq <- subset(term.freq, term.freq >=5)
google_df <- data.frame(term = names(term.freq), freq = term.freq)


#install.packages("ggplot2")
library("ggplot2")

###################################### Plot the words - BARCHART ########################################

View(apple_df)
View(samsung_df)
View(google_df)

sorted_apple_df <- apple_df[sort(apple_df$freq[1:20], decreasing= TRUE),]
sorted_samsung_df <- samsung_df[sort(samsung_df$freq[1:20], decreasing= TRUE),]
sorted_google_df <- google_df[sort(google_df$freq[1:20], decreasing= TRUE),]

ggplot(sorted_apple_df, aes(x=term, y=freq), title="Apple Barchart") + geom_bar(stat = "identity") + xlab("Terms") + ylab("Count") +coord_flip()
ggplot(sorted_samsung_df, aes(x=term, y=freq), title="Samsung Barchart") + geom_bar(stat = "identity") + xlab("Terms") + ylab("Count") +coord_flip()
ggplot(sorted_google_df, aes(x=term, y=freq), title="Google Barchart") + geom_bar(stat = "identity") + xlab("Terms") + ylab("Count") +coord_flip()


##################################################################################################

# which words are associated with 'Apple'?
findAssocs(apple_tdm, "Apple", 0.2)

# which words are associated with 'Samsung'?
findAssocs(samsung_tdm, "Samsung", 0.2)

# which words are associated with 'Google'?
findAssocs(google_tdm, "Samsung", 0.2)


####################################### WORDCLOUD METHOD 1 ######################################

#install.packages("wordcloud")
library("wordcloud")

ap <- as.matrix(apple_tdm)
# calculate the frequency of words and sort it by frequency
word.freq <- sort(rowSums(ap), decreasing = T)
wordcloud(words = names(word.freq), freq = word.freq, min.freq = 10,
          random.order = F, colors = brewer.pal(8, 'Set1'))

sa <- as.matrix(samsung_tdm)
# calculate the frequency of words and sort it by frequency
word.freq <- sort(rowSums(sa), decreasing = T)
wordcloud(words = names(word.freq), freq = word.freq, min.freq = 10,
          random.order = F, colors = brewer.pal(8, 'Set1'))

go <- as.matrix(google_tdm)
# calculate the frequency of words and sort it by frequency
word.freq <- sort(rowSums(go), decreasing = T)
wordcloud(words = names(word.freq), freq = word.freq, min.freq = 10,
          random.order = F, colors = brewer.pal(8, 'Set1'))

##############################################################################################

############################# Clustering ########################################


###### Apple ######

# remove sparse terms
apple_tdm2 <- removeSparseTerms(apple_tdm, sparse = 0.95)
apple_m2 <- as.matrix(apple_tdm2)
# cluster terms
distMatrix1 <- dist(scale(apple_m2))
fit1 <- hclust(distMatrix1, method = "ward.D")

plot(fit1)
rect.hclust(fit1, k = 6) # cut tree into 6 clusters

####### Samsung ######

# remove sparse terms
samsung_tdm2 <- removeSparseTerms(samsung_tdm, sparse = 0.95)
samsung_m2 <- as.matrix(samsung_tdm2)
# cluster terms
distMatrix2 <- dist(scale(samsung_m2))
fit2 <- hclust(distMatrix2, method = "ward.D")

plot(fit2)
rect.hclust(fit2, k = 6) # cut tree into 6 clusters

####### Google ######

# remove sparse terms
google_tdm2 <- removeSparseTerms(google_tdm, sparse = 0.95)
google_m2 <- as.matrix(google_tdm2)
# cluster terms
distMatrix3 <- dist(scale(google_m2))
fit3 <- hclust(distMatrix3, method = "ward.D")

plot(fit3)
rect.hclust(fit3, k = 4) # cut tree into 6 clusters


##########################################################################################################
################################ Topic Model #######################################

library(topicmodels)

apple_dtm <- as.DocumentTermMatrix(apple_tdm)
samsung_dtm <- as.DocumentTermMatrix(samsung_tdm)
google_dtm <- as.DocumentTermMatrix(google_tdm)

## 
apple_lda <- LDA(apple_dtm, k = 8) # find 8 topics
apple_term <- terms(apple_lda, 4) # first 4 terms of every topic
apple_term

samsung_lda <- LDA(samsung_dtm, k = 8) # find 8 topics
samsung_term <- terms(samsung_lda, 4) # first 4 terms of every topic
samsung_term

google_lda <- LDA(google_dtm, k = 8) # find 8 topics
google_term <- terms(google_lda, 4) # first 4 terms of every topic
google_term

##
require(data.table) #fore IDate
apple_term <- apply(apple_term, MARGIN = 2, paste, collapse = ", ")
apple_term

samsung_term <- apply(samsung_term, MARGIN = 2, paste, collapse = ", ")
samsung_term

google_term <- apply(google_term, MARGIN = 2, paste, collapse = ", ")
google_term
