test<-read.csv("Day24-pca.csv",sep=";",dec=",",stringsAsFactors = F)
test$index_name<-NULL
test$better_value<-NULL
library(tidyr)
library(zoo)
test<-test[-c(65564,65565),]
testDF<-spread(test,index_abbr,val)
summary(testDF)
sum(is.na(testDF))
for(i in c(unique(testDF$name_reg))){
  for(j in 4:ncol(testDF)){
    if (sum(is.na(testDF[testDF$name_reg==i,j]))!=length(testDF[testDF$name_reg==i,j])){
    testDF[testDF$name_reg==i,j]<-na.spline(testDF[testDF$name_reg==i,j])
    testDF[testDF$name_reg==i,j]<-na.approx(testDF[testDF$name_reg==i,j])
    }
  }
}
sum(is.na(testDF))
apply(testDF,2,function(x) sum(is.na(x)))/nrow(testDF)

testDF<-complete(mice(testDF[-c(1,2,3),],1,method = "rf"),1)
sum(is.na(testDF))
