#HW5
library(dplyr)
library(tidyr)
library(ggplot2)

#Slide 3
df<-read.csv("DataDay2.csv",sep=";",dec=",")
str(df)
df$GDP.per.capita<-abs(df$GDP.per.capita)
df$Area<-abs(df$Area)

#1
apply(df[3:6],2,shapiro.test)
#2
apply(df[3:6],2,function (x) {wilcox.test(x,mu=median(x,na.rm = T))})
#3
df%>%filter(Region!="North America")%>%group_by(Region)%>%summarise(p=shapiro.test(CO2.emission)$p.value)%>%arrange(desc(p))
#4
d<-df%>%group_by(Region)%>%summarise(Pop=sum(Populatiion,na.rm = T))
pie(d$Pop,labels =d$Region)


#Slide 6

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

model <- arima(newTS, order = c(1,1,5))
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

plot(forec)


#slide 7
#1
usd <- read.csv("Day5-EUR_USD.csv")
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
ar<-apply(tsUsd,2,auto.arima)
forec1<-forecast(HoltWinters(tsUsd[,1],gamma=F), h=10)
forec2<-forecast(HoltWinters(tsUsd[,2],gamma=F), h=10)
forec3<-forecast(HoltWinters(tsUsd[,3],gamma=F), h=10)
plot(forec1)
plot(forec2)
plot(forec3)
