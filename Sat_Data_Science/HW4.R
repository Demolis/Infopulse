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

#Slide y
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

df<-read.csv("Day4.csv",sep=";",dec=",")
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
test<-read.csv("Day4t.csv",sep=";",dec=",")
Cql<-lm1$coefficients[1]+lm1$coefficients[2]*test$Is+lm1$coefficients[3]*test$Ie+lm1$coefficients[4]*test$Iec
er1<-sum((test$Cql-Cql)^2)

Cql<-lm2$coefficients[1]+lm2$coefficients[2]*test$Is^2+lm2$coefficients[3]*test$Ie^2+lm2$coefficients[4]*test$Iec^2
er2<-sum((test$Cql-Cql)^2)

