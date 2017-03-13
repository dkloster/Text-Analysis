#Top Ten Word Co-occurrence, Curated Stopwords Removed

library(NLP)
library(tm)
library(SnowballC)


#Set the working directory
setwd("~/Text-Analysis/")


#Create a corpus 
corpus <- Corpus(DirSource("data/folgerComplete/"))

#Clean the corpus
corpus <- tm_map(corpus, content_transformer(tolower))
#To change the stopword list, use other dictionaries available with the tm package
#Add early modern stopwords
myStopWords <- scan("data/earlyModernStopword.txt", what="character", sep="\n")
corpus <- tm_map(corpus, removeWords, c(stopwords("english"), myStopWords))
corpus <- tm_map(corpus, removePunctuation)
corpus <- tm_map(corpus, stripWhitespace)
#corpus <- tm_map(corpus, PlainTextDocument)
corpus <- tm_map(corpus, stemDocument)

#Create matrix using DocumentTermMatrix function and saving it as "dtm"
#dtm <- DocumentTermMatrix(corpus)
#dtms <- removeSparseTerms(dtm, 0.2)

tdm <- TermDocumentMatrix(corpus)
tdms <- removeSparseTerms(tdm, 0.2)

#Find overall frequency 
freq <- sort(colSums(as.matrix(tdms)), decreasing = TRUE)

#Find results: NOTE: for this to work, you must first click the "Source" button in the source box and then run the findAssocs script in the Console on the bottom left in RStudio. It must be done in that order.
#findAssocs(tdms, "father", .6)
#findAssocs(tdms, "love", .6)
