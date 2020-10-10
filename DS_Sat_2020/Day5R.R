
library('dplyr')
library('tidyr')
library('ggplot2')

salaries_SF <- read.csv("Salaries.csv")

str(salaries_SF)
ssf <- salaries_SF

dc <- c('Id', 'Notes', 'Agency')
#ssf <- subset(ssf, select = -c(1, 11, 12))
ssf <- ssf %>% select( -one_of(dc))
# Names & Job Title
ssf_names <- select(ssf, c(1:2))


# Pay and Benefits
ssf_pay <- select(ssf, c(3:8)) %>% 
  mutate_if(is.factor, as.character) %>% 
  mutate_if(is.character, as.double)

ssf <- bind_cols(ssf_names, ssf_pay, select(ssf, c(9:10)))
ssfreg<-ssf[1:500,]
str(ssf)
xssf=ssf[!is.na(ssf$OtherPay),]$OtherPay
yssf=ssf[!is.na(ssf$OtherPay),]$TotalPay

#----------

shapiro.test(rnorm(5000))
shapiro.test(runif(5000,min=1,max=9))

shapiro.test(xssf)

shapiro.test(xssf[1:5000])

install.packages("nortest", dep=T)
library(nortest)
ad.test(xssf)

qqnorm(xssf)
qqline(xssf, col=1)

temp<-rnorm(5000)

qqnorm(temp)
qqline(temp, col=1)


median(xssf)
wilcox.test(xssf, mu=median(xssf), conf.int=T)

xssf_1=ssf[!is.na(ssf$TotalPay)&ssf$JobTitle=="Assistant Purchaser",]$TotalPay
xssf_2=ssf[!is.na(ssf$TotalPay)&ssf$JobTitle=="Assistant Retirement Analyst",]$TotalPay
t.test(xssf_1, xssf_2, var.equal= F) 
wilcox.test(xssf_1, xssf_2) 


install.packages("car", dep=T)
library(car) 
?scatterplotMatrix
scatterplotMatrix(~OvertimePay+OtherPay+BasePay, data=ssfreg,diagonal=list(method ="boxplot"))

install.packages("gridExtra",dep=T)
library(gridExtra)
p1 <- ggplot(aes(x= BasePay), data = ssf) + geom_histogram(color = 'black',  fill = '#F79420')
p2 <- p1 + scale_x_log10()
p3 <- p1 + scale_x_sqrt()
grid.arrange(p1, p2, p3, ncol = 1)

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




flu <- read.csv('http://www.google.org/flutrends/about/data/flu/ua/data.txt', 
                skip = 10)
head(flu)

plot(flu$Ukraine, type='l')
str(flu)
flu$Date <- as.Date(flu$Date)
str(flu)
plot(flu$Date,flu$Ukraine, type='l')

(flu$Date[1]-as.Date("2005-1-1"))/7
TS <- ts(flu$Ukraine, frequency = 52, start = c(2005,41))
str(TS)

dec <- decompose(TS) #type for multiplicative
plot(dec)
plot(TS)

acf(TS, lag.max = 48)
acf(TS-dec$seasonal, lag.max = 48)
Box.test(TS, type="Ljung-Box")

acf(ts(rnorm(100)))
Box.test(ts(rnorm(100)), type="Ljung-Box")

library(tseries)
adf.test(TS) # p-value < 0.05 indicates the TS is stationary

plot(diff(TS,2))

hist(diff(TS), breaks = 30)

install.packages("plm",dep=T)
library(forecast)
library(plm)

#exponential models
fit <- HoltWinters(TS, beta=FALSE, gamma=FALSE)
fit <- HoltWinters(TS, gamma=FALSE)
fit <- HoltWinters(TS)
plot(fit)
accuracy(forecast(fit))
forecast(fit, 10)
plot(forecast(fit, 4))



#aditive models
?arima
model <- arima(TS, order = c(10,1,1))
AIC(model)
model2 <- arima(TS, order = c(5,1,1))
AIC(model2)

exp((AIC(model2)-AIC(model))/2)

forec <- forecast(model2, h = 10)
forec$lower
plot(forec, xlim=c(2012,2016))

#automodels
# exponent
fit <- ets(TS)
fit <- stlf(TS)
# aditive
#fit <- auto.arima(TS)
