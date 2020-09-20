#HW 1 Day 1 Sl 7
#Task1 
a <- 5
b <- 7
s <- a*b
s
p <- 2*(a+b)
p

#Task2
a <- 2
b <- 1
res <- sqrt(a*b)
res

#Task3
x1 <- 3
y1 <- 2
x2 <- 1
y2 <- 0
a <- abs(x2 - x1)
b <- abs(y2 - y1)

s <- a*b
s
p <- 2*(a+b)
p

#Task4
A <- 98
logic_var <- A %% 2 != 0
logic_var

#Task5
A <- 3
B <- 9
C <- 2
logic_var <- (A < B) & (B < C)
logic_var

#Task6 
A <- 1990
B <- 4
C <- -1
logic_var <- (A > 0) | (B > 0) | (C > 0)
logic_var

#Task7

A <- 10
B <- -1
C <- -1
D <- 0
logic_var <- (A>0&B<=0&C<=0)|(A<=0&B>0&C<=0)|(A<=0&B<=0&C>0)

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
mas2

df<-read.csv("DataDay1.csv",sep=";",dec = ",")
df
str(df)
head(df,n = 5)
tail(df)
df$ISO<-NULL
df[df==NA]<-0
df[is.na(df)]<-0
tail(df)

df$GDP<-df$GDP.per.capita*df$Population
summary(df)
boxplot(df$GDP.per.capita)
plot(df$High.technology.exports,df$GDP)


