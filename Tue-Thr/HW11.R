#hw 11
library('dplyr')
library('tidyr')
library('ggplot2')
library(mice)
library(forecast)
#slide 1
#1
usd <- read.csv("Day11-EUR_USD.csv")
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
ar1<-arima(tsUsd[-(801:805),1], order = c(2,1,3))
ar2<-arima(tsUsd[-(801:805),2], order = c(2,1,3))
ar3<-arima(tsUsd[-(801:805),3], order = c(2,1,3))
forec1<-forecast(ar1, h=5)
forec2<-forecast(ar2, h=5)
forec3<-forecast(ar3, h=5)
plot(forec1,xlim=c(750,850))
lines(801:805,tsUsd[801:805,1])
plot(forec2,xlim=c(750,850))
lines(801:805,tsUsd[801:805,2])
plot(forec3,xlim=c(750,850))
lines(801:805,tsUsd[801:805,3])

#slide 3
#1
df <- read.csv("DataDay3.csv",dec=",",sep=";")
str(df)
df$GDP.per.capita<-abs(df$GDP.per.capita)
df$Area<-abs(df$Area)
#2
mean<-apply(df[,3:6],2,function (x) mean(x, na.rm=T))
sd<-apply(df[,3:6],2,function (x) sd(x, na.rm=T))
dfnorm<-df
for (i in 3:6)
  dfnorm[,i]<-(1+exp((mean[i-2]-dfnorm[,i])/sd[i-2]))^(-1)
mis<-mice(dfnorm[,3:6])
dfnorm<-complete(mis,2)
sum(is.na(dfnorm))
for (i in 3:6)
  df[,i]<-mean[i-2]-log(dfnorm[,i-2]^(-1)-1)*sd[i-2]
#3
df$Density<-df$CO2.emission/df$Populatiion*1000
mean[5]<-mean(df$Density)
sd[5]<-sd(df$Density)
lm1<-lm(df$GDP.per.capita~df$Density)
summary(lm1)
dfnorm<-df
for (i in 3:7)
  dfnorm[,i]<-(1+exp((mean[i-2]-dfnorm[,i])/sd[i-2]))^(-1)
lm2<-lm(dfnorm$GDP.per.capita~dfnorm$Density)
summary(lm2)

#4
res1<-lm1$coefficients[1]+lm1$coefficients[2]*2
res2<-lm2$coefficients[1]+lm2$coefficients[2]*(1+exp((mean[5]-2)/sd[5]))^(-1)
res2<-mean[1]-log(res2^(-1)-1)*sd[1]
res2
res1

#5
forsch<-dfnorm[dfnorm$Country.Name=="Ukraine"|dfnorm$Country.Name=="Poland",-2]
forsch<-gather(forsch, Var, Val, GDP.per.capita:Density)
ggplot(data=forsch,  aes(x=Var, y=Val, group=Country.Name, colour=Country.Name)) + 
  geom_point(size=2) + 
  xlab("") + 
  ylab("") + 
  ylim(0,1) + ggtitle("Data")  +
  coord_polar()
