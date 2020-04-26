install.packages(c("tm","wordcloud"),dep=T)
install.packages("SentimentAnalysis",dep=T)
library(tm)
library(ggplot2) 
library(RColorBrewer)
library(wordcloud)
library(SentimentAnalysis)


fpath <- "~/Documents/Work/InfoPulse/data-science/Saturday/Data/Texts/Learn/kant-metaphysical.txt"
fpath
dir(fpath)
a<-readLines(file(fpath))
b<-paste(a,collapse=" ")
a
?VCorpus

textL<- VCorpus(VectorSource(a)) 
summary(textL)
inspect(textL[1])

writeLines(as.character(textL[3]))



#Preprocessing

textL <- tm_map(textL,removePunctuation)   

textL <- tm_map(textL, removeNumbers) 
textL <- tm_map(textL, tolower)   

my<-c(stopwords("english"),"blabla")
stopwords("russian")
textL <- tm_map(textL, removeWords, stopwords("english"))   


textL <- tm_map(textL, stripWhitespace)
textL <- tm_map(textL, PlainTextDocument)

#Discovering
dtm <- DocumentTermMatrix(textL) #   TermDocumentMatrix


dtm$dimnames$Terms

freq <- colSums(as.matrix(dtm))   

freq
length(freq)

head(table(freq), 20)
tail(table(freq), 20)
findFreqTerms(dtm, lowfreq=25)  

wf <- data.frame(word=names(freq), freq=freq)   
head(wf)  

p <- ggplot(subset(wf, freq>40), aes(x = reorder(word, -freq), y = freq)) +
  geom_bar(stat = "identity") + 
  theme(axis.text.x=element_text(angle=45, hjust=1))   
p

#Relation between terms

a<-findAssocs(dtm, c("duty" , "moral"), corlimit=0.01)
a
set.seed(345)   
wordcloud(names(freq), freq, min.freq=25) 

set.seed(345)
wordcloud(names(freq), freq, min.freq=25, scale=c(5, .1), colors=brewer.pal(6, "Dark2"))   

dark2 <- brewer.pal(6, "Dark2")   
wordcloud(names(freq), freq, max.words=100, rot.per=0.2, colors=dark2)  

#Term Clustering
dtmss <- removeSparseTerms(dtm,sparse = 0.98) 
dtmss

library(fpc) 
library(cluster)
d <- dist(t(dtmss), method="euclidian")   
kfit <- kmeans(d, 3)   
clusplot(as.matrix(d), kfit$cluster, color=T, shade=T, labels=2, lines=0)  

#Sentiment analysis
?SentimentAnalysis
class_emo = analyzeSentiment(textL)
class_emo

convertToDirection(class_emo$SentimentGI)

