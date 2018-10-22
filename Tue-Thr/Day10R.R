
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

(flu$Date[1]-as.Date("2005-01-02"))/7
TS <- ts(flu$Ukraine, frequency = 52, start = c(2005,40))
str(TS)

dec <- decompose(TS)
plot(dec)
plot(TS)
plot(dec$trend,ylim=c(0,1000))
lines(TS)

hw <- HoltWinters(TS, beta = FALSE,gamma = FALSE)
plot(hw)

acf(TS, lag.max = 21)
Box.test(TS, type="Ljung-Box")

acf(ts(rnorm(100)))
Box.test(ts(rnorm(100)), type="Ljung-Box")

plot(diff(TS))

hist(diff(TS), breaks = 30)

install.packages("forecast",dep=T)
library(forecast)
library(plm)
?arima
model <- arima(TS, order = c(10,1,1))
AIC(model)
model2 <- arima(TS, order = c(5,1,1))
AIC(model2)

forec <- forecast(hw, h = 10)
plot(forec, xlim=c(2012,2016))
forec$lower
