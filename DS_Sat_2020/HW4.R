#HW4
library(dplyr)
library(tidyr)
library(ggplot2)
#Slide 1
df1<-read.csv("Day4-1.csv",sep=";",dec=",")
df2<-read.csv("Day4-2.csv",sep=";",dec=",")
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

#Slide 3
#1
df<-read.csv("Day4.csv",sep=";",dec=",")
summary(df)

klust1<-kmeans(df[,4:6],4,nstart = 20)
klust1
df$Clust1<-klust1$cluster
(df%>%arrange(desc(Iec)))$Clust1[1]

#2
plot(df$Ie,df$Iec,col=df$Clust1)
points(klust1$centers[,1],klust1$centers[,2],pch=5)

#3
klust2<-kmeans(df[,3],4,nstart = 20)
klust2
df$Clust2<-klust2$cluster
df%>%group_by(Clust1,Clust2)%>%summarise(n())

## here you can find your own associations 
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
df%>%group_by(Clust1,Region)%>%summarise(num=n())

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



#Slide 5

df<-read.csv("Day4.csv",sep=";",dec=",")
str(df)

#2
lm1<-lm(Cql~Is+Ie+Iec,data=df)
lm2<-lm(Cql~I(Is^2)+I(Ie^2)+I(Iec^2),data=df)
summary(lm1)
summary(lm2)
#3

test<-read.csv("Day4t.csv",sep=";",dec=",")
Cql<-lm1$coefficients[1]+lm1$coefficients[2]*test$Is+lm1$coefficients[3]*test$Ie+lm1$coefficients[4]*test$Iec
er1<-sum((test$Cql-Cql)^2)

Cql<-lm2$coefficients[1]+lm2$coefficients[2]*test$Is^2+lm2$coefficients[3]*test$Ie^2+lm2$coefficients[4]*test$Iec^2
er2<-sum((test$Cql-Cql)^2)

