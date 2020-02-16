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



ssfreg<-ssf[1:500,]
#----------Clasterization
ssfclust<-ssfreg[,1:5]
head(ssfclust)
kres=kmeans(ssfclust[,3:5],3,nstart=20)
kres
ssfclust$cluster<-kres$cluster
head(ssfclust)
kres$centers
kres
ggplot(data=ssfclust,aes(x=BasePay, y=OvertimePay))+
  geom_point(col=ssfclust$cluster)

for (i in 3:5){
  print(  qplot(x=ssfclust[,i]) + 
            geom_histogram()) 
}

install.packages("fpc", dep=T)
library(fpc)
ssfclust<-ssfreg[,1:5]
kres <- pamk(ssfclust[,3:5])
ssfclust$cluster<-kres$pamobject$clustering

ggplot(data=ssfclust,aes(x=BasePay, y=OvertimePay))+
  geom_point(col=ssfclust$cluster)
summary(ssfclust)
#--------
cor(ssfreg[,3:5])

mod1<-lm(OvertimePay~OtherPay+BasePay, data=ssf)
summary(mod1)

mod2<-lm(TotalPay~OvertimePay+OtherPay+BasePay, data=ssf)
summary(mod2)

install.packages("scatterplot3d", dep=T)
library(scatterplot3d)

s3d <- scatterplot3d(ssfreg$OtherPay, ssfreg$BasePay, ssfreg$TotalPay,highlight.3d = T, type = "h",
                     lab = c(2, 3)) # lab: number of tickmarks on x-/y-axes
s3d$plane3d(mod2$coefficients[1],mod2$coefficients[3],mod2$coefficients[4]) # draws the fitted plane lm

