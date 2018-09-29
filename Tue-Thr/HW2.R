#Slide 1
#1
matr<-matrix(rnorm(25),nrow =  5,ncol = 5)
#2
plot(matr[,which.min(apply(matr,2,mean))],
     matr[,which.max(apply(matr,2,sd))],col="red",type="l",xlab = "X", ylab = "Y")
#7
x<-1
y<-1
(x+y)%%2==0
#8
x1<-1
y1<-1
x2<-3
y2<-2
(x1==x2)||(y1==y2)||((x1-x2)==(y1-y2))

#slide 4

#1
my_seq<-function(a,b){
  return(list(seq=a:b,num=b-a-1))
  }
my_seq(1,15)
#2
my_n<-function(N){
  a<-0
  while(N>1){
    a<-a*10+N%%10
    N<-N%/%10
  }
  return(a)
}
my_n(342124)
#3
my_mas<-function(m){
  for(i in length(m)){
    if (m[i]>mean(m)){
      m[i]<-m[i]-18
    }
  }
  return(m)
}
my_mas(mas)
#4
apply(matr,2,hist)
#5
my_n<-function(N){
  if (N==2) return(F)
  for(i in 2:(N-1)){
    if (N%%i==0) return (F)
  }
  return(T)
}

