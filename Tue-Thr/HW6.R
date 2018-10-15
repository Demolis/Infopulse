#HW6
library(dplyr)
library(tidyr)
library(ggplot2)
library(outliers) 


#Slide 1
df1<-read.csv("Day5-1.csv",sep=";",dec=",")
df2<-read.csv("Day5-2.csv",sep=";",dec=",")
#1
cor(df1$Par.1,df1$Par.2)
cor(df2$Par.1,df2$Par.2)
#2
plot(df1)
plot(df2)
#3
lmodel<-lm(df1$Par.2~df1$Par.1)
summary(lmodel)

lmodel2<-lm(df2$Par.2~I(df2$Par.1^2)+df2$Par.1)
summary(lmodel2) #p>0.227

lmodel3<-lm(df2$Par.2~I(df2$Par.1^2))
summary(lmodel3)

#Slide 5
df<-read.csv("DataDay3.csv",sep=";",dec=",")
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
#5
boxplot(df$CO2.emission)
grubbs.test(df$CO2.emission, type = 10) 



