install.packages ("twitteR") #installs Twitter library (twitter #loads Twitter
library(twitteR)
library(rtweet)
library(tidyverse)
#CODE for Tweets Extraction
install.packages('tidyverse')
install.packages ("httk")
install.packages ("httr")
install.packages ("httpuv")
install.packages("rtweet")

consumer_key= 'Your_Consumer_key'
consumer_secret ='Your_consumer_secret'
access_token ='Your_access_token'
access_token_secret='Your_access_token_secret'

appname <- "Twitter_ADBMS_TS"
token < create_token(app =appname, consumer_key, consumer_secret,set_renv = TRUE)

lrt < lapply( 
c("Claritin", "#Claritin"),
search_tweets,
n = 7000, lang ="en")

rt9 <- do_call_rbind(lrt)
View(rt9)
rt9 < as.data.frame(rt9)
str(rt9)
write_as_csv(rt9,'/Users/akashadlakha/Desktop/UIC/Spring 18/Information Stratergy and Policy/Project/Claritin News 25.csv')
rt9 < rt9[!duplicated(rt9$text) ,]



#CODE: For Cleaning, stemming and document term frequency generator.
data <- read.csv(file.choose())

#Stemming and Frew words
docs <- Corpus(VectorSource(data$text))
docs <-tm_map(docs,content_transformer(tolower))

toSpace <- content_transformer(function(x, pattern) { return (gsub(pattern, " ", x))})

docs <- tm_map(docs, toSpace, "-")
docs <- tm_map(docs, toSpace, "'")
docs <- tm_map(docs, toSpace, "'")
docs <- tm_map(docs, toSpace, ".")
docs <- tm_map(docs, toSpace, """)
docs <- tm_map(docs, toSpace, """)
docs <- tm_map(docs, toSpace, "#")
docs <- tm_map(docs, toSpace, "<")
docs <- tm_map(docs, toSpace, ">")
docs <- tm_map(docs, toSpace, "@\\w+ *")
docs <- tm_map(docs, toSpace, "&\\w+ *")
docs <- tm_map(docs, toSpace, "[^\x01-\x7F]")
docs <- tm_map(docs, toSpace, "(f|ht)tp(s?)://(.*)[.][a-z]+")
docs <- tm_map(docs, toSpace, ":")

data$text[1]
writeLines(as.character(docs[[1]]))

#remove punctuation
docs <- tm_map(docs, removePunctuation)
#Strip digits
docs <- tm_map(docs, removeNumbers)
#remove stopwords
docs <- tm_map(docs, removeWords, stopwords("english"))
#remove whitespace
docs <- tm_map(docs, stripWhitespace)
#Good practice to check every now and then
writeLines(as.character(docs[[1]]))
#Stem document
docs1 <- docs

docs <- tm_map(docs,stemDocument, language = "english")
writeLines(as.character(docs[[1]]))

#Create document-term matrix
dtm <- DocumentTermMatrix(docs)
#convert rownames to filenames
#rownames(dtm) <- filenames
#collapse matrix by summing over columns
freq <- colSums(as.matrix(dtm))
#length should be total number of terms
length(freq)
#create sort order (descending)
ord <- order(freq,decreasing=TRUE)
#List all terms in decreasing order of freq and write to disk
freq[ord]

write.csv(freq[ord],"Result.csv") 	#CSV file with frequency of each word in stemmed form.

#CODE: For Matching of words with the Bag of Words
word.match <- function(sentences,list.words){
  
  scores<-laply(sentences,function(sentence,list.words){
    sentence<-gsub('[[:punct:]]',"",sentence)
    sentence<-gsub('[[:cntrl:]]',"",sentence)
    sentence<-gsub('\\d+',"",sentence)
    
    word.list<-str_split(sentence,'\\s+')
    words<-unlist(word.list)
    pos.matches<-match(words,list.words)
    
    pos.matches<-!is.na(pos.matches)
    score<-sum(pos.matches)
    return(score)
  },list.words)
  scores.df<-data.frame(score=scores,text=sentences)
  return(scores.df)
}

data <- read.csv(file.choose())
list.words<-scan('bag_of_words.csv', what = 'character', comment.char = '')

test.score<- word.match(claritin$text,list.words)

View(test.score)

Result <- test.score[test.score$score>0,]
write.csv(Result, "Result.csv")

  