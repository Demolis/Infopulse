## matrices
matr1 <- matrix (seq(1,16), nrow= 4, ncol =4)
matr1
rownames(matr1)<-c(1,2,3,"A")
matr1[2:4,-2]
arr_4<-cbind(arr_1,arr_2) #also rbind()
matr1[4,3:4]
t(matr1)

## lists
v1<-1:5
v2<- c(T,FALSE,F)
v3<-c("Hello","world")
lst<-list(First=v1, Logic=v2, Text=v3)
lst
str(lst)
lst$First
lst$First[3]
lst[[2]][1]

## data frames
getwd()
setwd("D:/Work/InfoPulse/data-science/Saturday/Data")
tbl <- read.csv("1.csv",sep=";")
tbl
str(tbl)
head(tbl)
tail(tbl,n = 2)
tbl$Sum<-tbl$Par.1+tbl$Par.2

#functions
i<-{a<-2; b<-3; a; a+b}

summa <-function(a,b){
  return(a+b)
}

summa(4, b=2)

summa2 <-function(a,b=1){
  print(a+b)
}
summa2(4)

matr1 <- matrix (seq(1,16), nrow= 4, ncol =4)
apply(matr1,1,sum)
apply(matr1,2,sum)

apply(matr1,2,function(x) summa(x,3))

#if clause
if (a>b){
  print("a > b")
} else {
  print ("a < b")
}

#cycles
for (i in 1:10){
  print(i)
}
num<-0
mas<-num
while(num<=10){
  num<-num+1
  mas<-c(mas,num)
}
mas