install.packages(c("tm","wordcloud"),dep=T)
install.packages("SentimentAnalysis",dep=T)
library(tm)
library(ggplot2) 
library(RColorBrewer)
library(wordcloud)
library(SentimentAnalysis)


fpath <- "~/Documents/Work/InfoPulse/data-science/Datasets/Texts/Learn/kant-metaphysical.txt"
fpath
dir(fpath)
a<-readLines(file(fpath))
a
a<-paste(a,collapse = "")

textL<- VCorpus(VectorSource(a)) 


#Preprocessing

textL <- tm_map(textL,removePunctuation)   

textL <- tm_map(textL, removeNumbers) 
textL <- tm_map(textL, tolower)   

stopwords("english")
stopwords("russian")
textL <- tm_map(textL, removeWords, stopwords("english"))   


textL <- tm_map(textL, stripWhitespace)
textL <- tm_map(textL, PlainTextDocument)


#Discovering
dtm <- DocumentTermMatrix(textL) #   TermDocumentMatrix

dtm$dimnames$Terms
set.seed(100)
install.packages(c("topicmodels","SnowballC"))
library(topicmodels)
library(SnowballC)
burnin = 2000
iter = 10000
keep = 50
res<-LDA(dtm,
    k = 5,
    method = "Gibbs",
    control = list(burnin = burnin,
                   iter = iter,
                   keep = keep,seed=100))
res@logLiks[-c(1:(burnin/keep))]
str(res@logLiks)
1/mean(1/res@logLiks[-c(1:(burnin/keep))])
topics(res,5) 
terms(res,10)

#######
temp<-strsplit(textL[[1]]$content," ")[[1]]
temp<-wordStem(temp,language="ru")
textL[[1]]$content<-paste(temp,collapse=" ")

textL[[1]]$content<-stemDocument(textL[[1]]$content,language="en")
