#HW6
library(dplyr)
library(tidyr)
library(ggplot2)

#Slide 2
df<-read.csv("DataDay3.csv",sep=";",dec=",")
summary(df)
df$GDP.per.capita<-abs(df$GDP.per.capita)
df$Area<-abs(df$Area)

#1
cor(df[3:6],use = "complete")
#2
df$CO2perCapita<-df$CO2.emission/df$Populatiion
df$Density<-df$Populatiion/df$Area
p1<-ggplot(df,aes(x=CO2perCapita,y=GDP.per.capita))+geom_point()
p1
p1+xlim(0,0.025)
p2<-ggplot(df,aes(x=Density,y=GDP.per.capita))+geom_point()
p2
p2+xlim(0,2000)
#3
m1<-lm(df$GDP.per.capita~df$CO2perCapita)
m2<-nls(GDP.per.capita~a+b/(Density),data=df,start = list(a=100,b=2))
summary(m1)
summary(m2)
#4
mean(df$GDP.per.capita,na.rm = T)
m1$coefficients[1]+m1$coefficients[2]*mean(df$CO2perCapita,na.rm = T)
m2$m$getPars()[1]+m2$m$getPars()[2]/mean(df$Density,na.rm = T)

#Slide 4

df<-read.csv("DataDay6.csv",sep=";",dec=",")
str(df)
#1
cor(df[5:7])
library(car) 
scatterplotMatrix(~Is+Ie+Iec, data=df,diagonal=list(method ="boxplot"))
#2
lm1<-lm(Cql~Is+Ie+Iec,data=df)
lm2<-lm(Cql~I(Is^2)+I(Ie^2)+I(Iec^2),data=df)
summary(lm1)
summary(lm2)
#3
test<-read.csv("DataDay6t.csv",sep=";",dec=",")
Cql<-lm1$coefficients[1]+lm1$coefficients[2]*test$Is+lm1$coefficients[3]*test$Ie+lm1$coefficients[4]*test$Iec
er1<-sum((test$Cql-Cql)^2)

Cql<-lm2$coefficients[1]+lm2$coefficients[2]*test$Is^2+lm2$coefficients[3]*test$Ie^2+lm2$coefficients[4]*test$Iec^2
er2<-sum((test$Cql-Cql)^2)

