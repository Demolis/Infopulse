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

ssf_names %>% 
  mutate_all(funs(factor(.)))

# Pay and Benefits
ssf_pay <- select(ssf, c(3:8)) %>% 
  mutate_if(is.factor, as.character) %>% 
  mutate_if(is.character, as.double)

ssf <- bind_cols(ssf_names, ssf_pay, select(ssf, c(9:10)))

#-------------
xssf=ssf[!is.na(ssf$OtherPay),]$OtherPay
yssf=ssf[!is.na(ssf$OtherPay),]$TotalPay

reslm<-lm(formula = yssf ~ xssf)
summary (reslm)

a0 <- reslm$coefficient[1]
a1 <- reslm$coefficient[2]
xmin <- min(xssf)
xmax <- max(xssf)
x <- seq(from = xmin, to = xmax, length.out =100)
y <- a0 + a1*x
str(x)

plot(xssf, yssf, main="", xlab="Other Pay", ylab="Total Pay")
grid()
lines(x, y, col="red")



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

mod1<-lm(TotalPay~OtherPay+BasePay+OvertimePay, data=ssf)
summary(mod1)


install.packages("scatterplot3d", dep=T)
library(scatterplot3d)
s3d <- scatterplot3d(ssfreg$OtherPay, ssfreg$BasePay, ssfreg$OvertimePay,highlight.3d = T, type = "h",
                      lab = c(2, 3)) # lab: number of tickmarks on x-/y-axes
s3d$plane3d(mod1) # draws the fitted plane lm




