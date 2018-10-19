#hw 6
library('dplyr')
library('tidyr')
library('ggplot2')
library(mice)
library(forecast)
#slide 1
#1
usd <- read.csv("Day6-EUR_USD.csv")
str(usd)
#2
plot(usd$Price,col=14,type="l")
lines(usd$High)
lines(usd$Low,pch=5)
  
#3
tsUsd<-ts(usd[,c(2,4,5)])
plot(tsUsd)
decompose(tsUsd)  
#4
ar<-apply(tsUsd,2,function(x) arima(x, order = c(2,1,3)))
forec<-apply(tsUsd,2,function(x) forecast(x, h=10))
plot(forec[[1]])
plot(forec[[2]])
plot(forec[[3]])

#slide 4
#1
titan <- read.csv("Day6-titanic.csv")
str(titan)
head(titan)
#2
mean(titan[titan$Survived==1,"Age"],na.rm = T)
#3
shapiro.test(titan[titan$Survived==0,"Age"])
#4
titan[titan$Survived==1,]%>%group_by(Embarked)%>%summarise(n())
#5
pie(t((titan%>%group_by(Pclass)%>%summarise(n()))[2]))
#6
cor(titan$Age,titan$Fare,use="pairwise")
plot(titan$Age,titan$Fare)
#7
md.pattern(titan)
titan$Age=na.spline(titan$Age)
#8
clust<-kmeans(titan[,c(6,10,2)],3,nstart = 20)
titan$clust<-clust$cluster
res<-titan%>%group_by(clust,Pclass)%>%summarise(n())

for (i in 1:3)
print(res[res$clust==i,3]/sum(res[res$clust==i,3])*100)
