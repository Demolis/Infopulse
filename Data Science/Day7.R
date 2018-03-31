#from day 5
library('dplyr')
library('tidyr')
library('ggplot2')
#library('gridExtra')

salaries_SF <- read.csv("Salaries.csv")

str(salaries_SF)
ssf <- salaries_SF

dc <- c('Id', 'Notes', 'Agency')
#ssf <- subset(ssf, select = -c(1, 11, 12))
ssf <- ssf %>% select( -one_of(dc))
# Names & Job Title
ssf_names <- select(ssf, c(1:2))

ssf_names %>% 
  mutate_all(funs(factor(.)))

# Pay and Benefits
ssf_pay <- select(ssf, c(3:8)) %>% 
  mutate_if(is.factor, as.character) %>% 
  mutate_if(is.character, as.double)

ssf <- bind_cols(ssf_names, ssf_pay, select(ssf, c(9:10)))
ssfreg<-ssf[1:500,]

#----------Clasterization
ssfclust<-ssfreg[,1:5]
kres=kmeans(ssfclust[,3:5],3)
ssfclust$cluster<-kres$cluster
kres$centers
ggplot(data=ssfclust,aes(x=BasePay, y=OvertimePay))+geom_point(col=ssfclust$cluster)



install.packages("fpc", dep=T)
library(fpc)
ssfclust<-ssfreg[,1:5]
kres <- pamk(ssfclust[,3:5])
ssfclust$cluster<-kres$pamobject$clustering

ggplot(data=ssfclust,aes(x=BasePay, y=OvertimePay))+geom_point(col=ssfclust$cluster)


#----------Loops, functions
is.numeric(ssfreg)

for(i in 1:ncol(ssfreg)){
  print(is.numeric(ssfreg[,i]))
}

lapply(ssfreg,is.numeric)

f<-function (par=2){
  print(par)
}
f()

f(23)

#----------Date, time serries
?Date
Sys.time()
Sys.Date()
dates <- c("05/27/84", "07/07/05")
bd <- as.Date(dates, format = "%m/%d/%y")
bd

dates <- c(30829, 38540)
bd <- as.Date(dates,origin = "1899-12-30")
bd

format(bd,"%a %b %d")
weekdays(bd)
mean(bd)




flu <- read.csv('http://www.google.org/flutrends/about/data/flu/ua/data.txt', skip = 10)
head(flu)

plot(flu$Ukraine, type='l')
str(flu)
flu$Date <- as.Date(flu$Date)
str(flu)
plot(flu$Date,flu$Ukraine, type='l')

flu$Date[1]
TS <- ts(flu$Ukraine, frequency = 52, start = c(2005,10,9))
str(TS)

dec <- decompose(TS)
plot(dec)
plot(TS)
plot(TS - dec$seasonal)

hw <- HoltWinters(TS, beta = FALSE, gamma = FALSE)
plot(hw)

acf(TS, lag.max = 52)
Box.test(TS, lag=52, type="Ljung-Box")

acf(ts(rnorm(100)))
Box.test(ts(rnorm(100)), type="Ljung-Box")

plot(diff(TS))

hist(diff(TS), breaks = 30)

install.packages("plm",dep=T)
library(forecast)
library(plm)
model <- arima(TS, order = c(2,1,1))
summary(model)
forec <- forecast(model, h = 10)
plot(forec, xlim=c(2012,2016))
