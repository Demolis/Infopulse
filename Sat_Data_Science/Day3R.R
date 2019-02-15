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

ssf
ggplot(aes(x = BasePay , y = TotalPayBenefits), data = ssf) +
  geom_point()

ggplot(aes(x = BasePay, y = TotalPayBenefits), data = ssf) +
  geom_jitter(alpha = 0.05)

ggplot(aes(x = BasePay, y = TotalPayBenefits), data = subset(ssf, TotalPayBenefits > 0)) + 
  geom_jitter(alpha = 0.05) +
  coord_trans(y = "sqrt")

ggplot(aes(x = BasePay, y = TotalPayBenefits), data = subset(ssf, TotalPayBenefits > 0)) + 
  geom_point(alpha = 0.01, position = ?position_jitter(h = 0)) +
  coord_trans(y = "log")

ssf.BasepayYoY <- subset(ssf, !is.na(BasePay)) %>%
  group_by(Year) %>%
  summarise(bp_mean = mean(BasePay),
            bp_median = median(BasePay),
            number = n()) %>%
  arrange(Year)



ggplot( data = ssf.BasepayYoY) +
  geom_line(aes(x= Year, y= bp_median, color=bp_median), show.legend = FALSE) +
  geom_line(aes(x= Year, y= bp_mean, color=bp_mean), show.legend = FALSE) +
  labs(y = "USD", title = "Mean and Median for SF Salaries, 2011-2014")  


#-------------
str(ssf)
xssf=ssf[!is.na(ssf$OtherPay),]$OtherPay
yssf=ssf[!is.na(ssf$OtherPay),]$TotalPay
cor(xssf,yssf)
cor(xssf, yssf, method="spearman")

ggplot(aes(x = OtherPay, y = TotalPay), data=ssf) +
  geom_point(color="red")

cor.test(xssf,yssf)

corr_str<-c("BasePay","OvertimePay","TotalPay","TotalPayBenefits")
matr_corr <- cor(ssf[corr_str],use = "complete") 
matr_corr

install.packages("ellipse", dep=T)
library(ellipse) 
plotcorr(cor(ssf[corr_str],use = "complete") )  


#--------

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


