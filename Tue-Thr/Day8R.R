
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
ssfreg<-ssf[1:500,]

#----------Clasterization
ssfclust<-ssfreg[,1:5]
head(ssfclust)
kres=kmeans(ssfclust[,3:5],3, nstart=20)
kres
ssfclust$cluster<-kres$cluster
head(ssfclust)
kres$centers
ggplot(data=ssfclust,aes(x=BasePay, y=OvertimePay))+
  geom_point(col=ssfclust$cluster)

install.packages("fpc", dep=T)
library(fpc)
ssfclust<-ssfreg[,1:5]
kres <- pamk(ssfclust[,3:5])
ssfclust$cluster<-kres$pamobject$clustering

ggplot(data=ssfclust,aes(x=BasePay, y=OvertimePay))+
  geom_point(col=ssfclust$cluster)


#============================
install.packages("VIM",dep=T)
library(VIM)
?sleep
data(sleep, package="VIM")
sleep[complete.cases(sleep),] 
sleep[!complete.cases(sleep),]
str(sleep)

install.packages("mice",dep=T)
library(mice)
md.pattern(sleep) 
na.omit(sleep)
?cor(sleep, use="pairwise.complete.obs")

imp <- mice(sleep, seed=1234) #method – parameter of method
imp$imp
imp$m
res <- complete(imp, action=2) 
res  
md.pattern(res) 

library(zoo)
na.approx(sleep,na.rm=F)
na.spline(sleep,na.rm=F)

