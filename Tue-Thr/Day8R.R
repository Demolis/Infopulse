
library('dplyr')
library('tidyr')
library('ggplot2')


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

