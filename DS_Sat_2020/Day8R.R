library(highcharter)

#http://jkunst.com/highcharter/highmaps.html 
#https://code.highcharts.com/mapdata/
  
hcmap("countries/fr/custom/fr-all-mainland", showInLegend = FALSE)%>%
  hc_add_series(data = data.frame(name = c(1,2,3,4),
                                  lat = c(45.0, 47.2,45.5, 48.5),
                                  lon = c(0, 3.0, 7.0, -2.0),
                                  z=c(1,1,1,1)), type = "mapbubble", name = "Data",maxSize="1%") %>% 
  hc_mapNavigation(enabled = TRUE) 

mapdata <- get_data_from_map(download_map_data("countries/fr/fr-all-all"))
library(dplyr)
head(mapdata)
data_fake <- mapdata %>% 
  select(code = `hc-a2`) %>% 
  mutate(value = rnorm(nrow(.),1000,500))

hcmap("countries/fr/fr-all-all", data = data_fake, value = "value",
      joinBy = c("hc-a2", "code"), name = "Fake data",
      dataLabels = list(enabled = TRUE, format = '{point.name}'),
      borderColor = "#FAFAFA", borderWidth = 0.1,
      tooltip = list(valueDecimals = 2, valuePrefix = "$", valueSuffix = " mln")) %>% 
  hc_mapNavigation(enabled = TRUE) 


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

#Sentiment analysis
?SentimentAnalysis
class_emo = analyzeSentiment(textL)
class_emo

convertToDirection(class_emo$SentimentGI)

#Context analysis
dtm$dimnames$Terms

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
                        keep = keep))
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

