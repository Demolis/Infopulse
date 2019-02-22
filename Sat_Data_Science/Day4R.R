#from day 3
library(dplyr)
library(tidyr)
library(ggplot2)


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

median(xssf)
wilcox.test(xssf, mu=median(xssf), conf.int=T)

xssf_1=ssf[!is.na(ssf$TotalPay)&ssf$JobTitle=="Assistant Purchaser",]$TotalPay
xssf_2=ssf[!is.na(ssf$TotalPay)&ssf$JobTitle=="Assistant Retirement Analyst",]$TotalPay
?t.test(xssf_1, xssf_2, var.equal= F) 
wilcox.test(xssf_1, xssf_2) 

ssfreg<-ssf[1:500,]

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


cor(ssfreg[,3:5])

mod1<-lm(OvertimePay~OtherPay+BasePay, data=ssf)
summary(mod1)

mod2<-lm(TotalPay~OvertimePay+OtherPay+BasePay, data=ssf)
summary(mod2)

install.packages("scatterplot3d", dep=T)
library(scatterplot3d)
s3d <- ?scatterplot3d(ssfreg$OtherPay, ssfreg$BasePay, ssfreg$TotalPay,highlight.3d = T, type = "h",
                     lab = c(2, 3)) # lab: number of tickmarks on x-/y-axes
s3d$plane3d(mod2$coefficients[1],mod2$coefficients[3],mod2$coefficients[4]) # draws the fitted plane lm

