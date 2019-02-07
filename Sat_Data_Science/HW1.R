#HW 1 Day 1 Sl 10

mas1<-rnorm(100,0,5)
mas2<-rnorm(100,10, sd = 27)
mean(mas1)
mean(mas2)
sd(mas1)
sd(mas2)
plot(mas1,mas2)
hist(mas1)
hist(mas2)
mas1*3
mas2[mas2>mean(mas2)]<-mas2[mas2>mean(mas2)]-18


df<-read.csv("DataDay1.csv",sep=";",dec = ",")
df
str(df)
head(df,n = 5)
tail(df)
df$ISO<-NULL

df[is.na(df)]<-0
tail(df)

df$GDP<-df$GDP.per.capita*df$Population
summary(df)
boxplot(df$GDP.per.capita)
plot(df$High.technology.exports,df$GDP)


