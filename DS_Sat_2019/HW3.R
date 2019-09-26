#HW Day 3 Slide 2
library(dplyr)
library(tidyr)
library(ggplot2)
#1
df1<-read.csv("DataDay3a.csv",sep=";")
df2<-read.csv("DataDay3b.csv",sep=";")
str(df1)
#2
df<-left_join(df1,df2,by="id")
str(df)
df<-df[,-(15:21)]
#3
df$Sum<-df$AveragePrice*df$Total.Volume
#4
df%>%group_by(type)%>%summarise(sum(Sum))
#5
df%>%group_by(year)%>%summarise(Sum=sum(Sum))%>%
  arrange(desc(Sum))
#6
plot(df$AveragePrice,df$Small.Bags)
plot(df$AveragePrice,df$Large.Bags)
plot(df$AveragePrice,df$XLarge.Bags)
#7
boxplot(df$Total.Volume)
#8
pie(apply(df%>%filter(year=="2016")%>%select(5:7),2,sum))
#9
df%>%group_by(region)%>%summarise(MeanP=mean(AveragePrice))%>%
  arrange(desc(MeanP))
df%>%group_by(region)%>%summarise(MeanP=mean(AveragePrice))%>%
  arrange(MeanP)
#10
#We will discuss it at the lesson 

#Slide 7
df<-read.csv("DataDay2.csv",sep=";",dec = ",")
names(df)[4]<-"Population"
summary(df)
df$GDP.per.capita<-abs(df$GDP.per.capita)
df$Area<-abs(df$Area)
#2
for (i in 3:5)
  for (j in (i+1):6){
  
    plot(df[,i],df[,j],xlab = colnames(df)[i], ylab = colnames(df)[j])
  }
#3
cor(df[,3:6],use = "pairwise.complete.obs")
#4 one of the example
df$co2my<-df$CO2.emission/df$Population
ggplot(aes(x = CO2.emission/Population , y = GDP.per.capita), data = df) +
  geom_point()

reslm<-lm(formula = df$GDP.per.capita ~ df$co2my)
summary (reslm)
reslm<-nls(formula = GDP.per.capita ~ a*co2my^2+b*co2my+c, data=df,start=list(a=1,b=1, c=1))
summary (reslm)
