#HW8
library(dplyr)
library(tidyr)
library(ggplot2)

#Slide 2
#1
df<-read.csv("DataDay6.csv",sep=";",dec=",")
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
df<-read.csv("DataDay3.csv",sep=";",dec=",")
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

for (i in 3:6){
  hist(df[,i],main=colnames(df)[i])
}

#6

myfunc<-function(a,b){
  if(cor(a,b)>0.8) return(T)
  return(F)
}
myfunc(c(1,2,3,4),c(2,4,6,8))


