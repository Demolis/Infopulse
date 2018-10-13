#HW5
library(dplyr)
library(tidyr)
library(ggplot2)

#Slide 2
#1
df<-read.csv("Day4.csv",sep=";",dec=",")
summary(df)

klust1<-kmeans(df[,5:7],4,nstart = 20)
klust1
df$Clust1<-klust1$cluster
(df%>%arrange(desc(Iec)))$Clust1[1]

#2
plot(df$Ie,df$Iec,col=df$Clust1)
points(klust1$centers[,1],klust1$centers[,2],pch=2)

#3
klust2<-kmeans(df[,4],4,nstart = 20)
klust2
df$Clust2<-klust2$cluster
df%>%group_by(Clust1,Clust2)%>%summarise(n())

df[df$Clust2==1,]$Clust2<-5
df[df$Clust2==3,]$Clust2<-1
df[df$Clust2==5,]$Clust2<-3
df[df$Clust2==2,]$Clust2<-5
df[df$Clust2==4,]$Clust2<-2
df[df$Clust2==5,]$Clust2<-4
sum((df$Clust1-df$Clust2)!=0)

#4
df<-read.csv("DataDay2.csv",sep=";",dec=",")
df$Area<-abs(df$Area)
df$GDP.per.capita<-abs(df$GDP.per.capita)
df$Density<-df$Populatiion/df$Area
str(df)
df$Density[is.na(df$Density)]<-mean(df$Density,na.rm = T)
df$GDP.per.capita[is.na(df$GDP.per.capita)]<-mean(df$GDP.per.capita,na.rm = T)
klust1<-kmeans(df[,c(3,7)],4,nstart = 20)
df$Clust1<-klust1$cluster
df%>%group_by(Clust1,Region)%>%summarise(num=n())%>%group_by(Clust1)

#5
mas<-c()
for (i in 3:6){
  hist(df[,i],main=colnames(df)[i])
}

#6

myfunc<-function(a,b){
  if(cor(a,b)>0.8) return(T)
  return(F)
}
myfunc(c(1,2,3,4),c(2,4,6,8))


#Slide 4

#1
fluUkr <- read.csv('http://www.google.org/flutrends/about/data/flu/ua/data.txt', 
                skip = 10)
fluPl<-read.csv('http://www.google.org/flutrends/about/data/flu/pl/data.txt', 
                skip = 10)

head(fluUkr)

fluUkr$Date <- as.Date(fluUkr$Date)
fluPl$Date <- as.Date(fluPl$Date)
plot(fluUkr$Date,fluUkr$Ukraine, type='l',ylim=c(0,1400))
lines(fluPl$Date,fluPl$Poland,col="red")
fluPl$Date[1]
TS <- ts(fluPl$Poland, frequency = 52, start = c(2004,09,26))


dec <- decompose(TS)
plot(dec)

acf(TS, lag.max = 52)
Box.test(TS, type="Ljung-Box")

library(forecast)
library(plm)
newTS<-TS-dec$seasonal
mas<-c()
for (i in 0:10){
  model <- arima(newTS, order = c(0,0,i))
  mas<-c(mas,AIC(model))  
}
plot(mas)
mas<-c()
for (i in 0:10){
  model <- arima(newTS, order = c(i,0,5))
  mas<-c(mas,AIC(model))  
}
plot(mas)
mas<-c()
for (i in 0:10){
  model <- arima(newTS, order = c(1,i,5))
  mas<-c(mas,AIC(model))  
}
plot(mas)

model <- arima(newTS, order = c(1,1,6))
AIC(model)
forec <- forecast(model, h = 10)
plot(forec, xlim=c(2012,2016))

dec$figure
season<-c(dec$figure[49:52],dec$figure[1:6])
forec$mean<-forec$mean+season
forec$x<-forec$x+dec$seasonal
forec$lower[,1]<-forec$lower[,1]+season
forec$lower[,2]<-forec$lower[,2]+season
forec$upper[,1]<-forec$upper[,1]+season
forec$upper[,2]<-forec$upper[,2]+season
?ts
plot(forec)
