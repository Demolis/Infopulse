install.packages(c("tm","wordcloud"),dep=T)
install.packages("SentimentAnalysis",dep=T)
library(tm)
library(ggplot2) 
library(RColorBrewer)
library(wordcloud)
library(SentimentAnalysis)


fpath <- "D:/Work/InfoPulse/data-science/Datasets/Texts/Learn"
fpath
dir(fpath)

?VCorpus
textL<- VCorpus(DirSource(fpath)) 
summary(textL)
inspect(textL[1])

writeLines(as.character(textL[1]))

#Preprocessing

textL <- tm_map(textL,removePunctuation)   
writeLines(as.character(textL[1]))

for (j in seq(textL)) {
  textL[[j]] <- gsub("–", " ", textL[[j]])
  textL[[j]] <- gsub("”", " ", textL[[j]])
}

textL <- tm_map(textL, removeNumbers) 
textL <- tm_map(textL, tolower)   
textL <- tm_map(textL, PlainTextDocument)
textLCopy <- textL

stopwords("english")
stopwords("russian")
textL <- tm_map(textL, removeWords, stopwords("english"))   
textL <- tm_map(textL, PlainTextDocument)


textL <- tm_map(textL, removeWords, c("will","fuck"))

for (j in seq(textL)){
  textL[[j]] <- gsub("annexation  crimea", "annexation_crimea", textL[[j]])
}
textL <- tm_map(textL, PlainTextDocument)


docs_st <- tm_map(textL, stemDocument)   
docs_st <- tm_map(docs_st, PlainTextDocument)
writeLines(as.character(docs_st[1]))


docs_stc <- tm_map(docs_st, stemCompletion, dictionary = textLCopy, lazy=TRUE)
docs_stc <- tm_map(docs_stc, PlainTextDocument)
writeLines(as.character(docs_stc[1])) 


textL <- tm_map(textL, stripWhitespace)
textL <- tm_map(textL, PlainTextDocument)

#Discovering
dtm <- DocumentTermMatrix(textL) #   TermDocumentMatrix
dtm
dtm$dimnames$Terms

freq <- colSums(as.matrix(dtm))   

freq
length(freq)
ord <- order(freq)   
ord




head(table(freq), 20)
tail(table(freq), 20)
findFreqTerms(dtm, lowfreq=5)  

wf <- data.frame(word=names(freq), freq=freq)   
head(wf)  

p <- ggplot(subset(wf, freq>3), aes(x = reorder(word, -freq), y = freq)) +
  geom_bar(stat = "identity") + 
  theme(axis.text.x=element_text(angle=45, hjust=1))
p   


#Relation between terms

a<-findAssocs(dtm, c("ukrainian" , "babchenko"), corlimit=0.85)

set.seed(345)   
wordcloud(names(freq), freq, min.freq=3) 

set.seed(345)
wordcloud(names(freq), freq, min.freq=3, scale=c(5, .1), colors=brewer.pal(6, "Dark2"))   

dark2 <- brewer.pal(6, "Dark2")   
wordcloud(names(freq), freq, max.words=100, rot.per=0.2, colors=dark2)  

#Term Clustering
dtmss <- removeSparseTerms(dtm, 0.1) 
dtmss

library(fpc) 
library(cluster)
d <- dist(t(dtmss), method="euclidian")   
kfit <- kmeans(d, 2)   
clusplot(as.matrix(d), kfit$cluster, color=T, shade=T, labels=2, lines=0)  

#Sentiment analysis
?SentimentAnalysis
class_emo = analyzeSentiment(textL)
class_emo
convertToDirection(class_emo$SentimentGI)

