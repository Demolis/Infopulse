#Slide 1
#1
matr<-matrix(rnorm(2500),nrow =  50,ncol = 50)
#2
plot(matr[,which.min(apply(matr,2,mean))],
     matr[,which.max(apply(matr,2,sd))],col="red",type="l",xlab = "X", ylab = "Y")
#3
x<-1
y<-1
(x+y)%%2==0
#4
x1<-1
y1<-1
x2<-3
y2<-2
(x1==x2)||(y1==y2)||((x1-x2)==(y1-y2))

#slide 3
#1
mas<-rnorm(100,10,6)
cut(mas,breaks=c(min(mas),mean(mas)-2*sd(mas),mean(mas)+2*sd(mas), 
                 max(mas)), labels = c("First","Second","Third"),
    include.lowest = T)
#2
my_seq<-function(a,b){
  return(list(seq=a:b,num=b-a-1))
  }
my_seq(1,15)
#3
my_n<-function(N){
  a<-0
  while(N>1){
    a<-a*10+N%%10
    N<-N%/%10
  }
  return(a)
}
my_n(342124)
#4
my_mas<-function(m){
  for(i in length(m)){
    if (m[i]>mean(m)){
      m[i]<-m[i]-18
    }
  }
  return(m)
}
my_mas(mas)
#5
apply(matr,2,hist)
#6
my_n<-function(N){
  if (N==2) return(F)
  for(i in 2:(N-1)){
    if (N%%i==0) return (F)
  }
  return(T)
}

#slide 5
#1
df<-read.csv("DataDay2.csv",sep=";",dec = ",")
#2
str(df)
#3
names(df)[4]<-"Population"
summary(df)
df$GDP.per.capita<-abs(df$GDP.per.capita)
df$Area<-abs(df$Area)
#4
apply(df[,3:6],2,hist)
apply(df[,3:6],2,boxplot)
#5
df$Density<-df$Population/df$Area

#------
#1
sum(as.integer(is.na(df)))

tdf<-apply(df[,3:6],2,function(x){x[is.na(x)]<-mean(x,na.rm = T);return(x)})
df[,3:6]<-tdf
#2
df[df$GDP.per.capita==max(df$GDP.per.capita),][1]
df[df$Area==min(df$Area),][1]
#3
df%>%group_by(Region)%>%summarise(mean=mean(Area))%>%arrange(mean)
#4
df%>%group_by(Area)%>%summarise(num=n())%>%arrange(desc(num))
df%>%filter(Region=="Europe & Central Asia")%>%group_by(Area)%>%summarise(num=n())%>%arrange(desc(num))
#5
df%>%group_by(Region)%>%summarise(mean=mean(GDP.per.capita),median=median(GDP.per.capita))
#6
df%>%arrange(GDP.per.capita)%>%select(Country.Name)%>%head(5)
df%>%arrange(GDP.per.capita)%>%select(Country.Name)%>%tail(5)
df%>%arrange(CO2.emission)%>%select(Country.Name)%>%head(5)
df%>%arrange(CO2.emission)%>%select(Country.Name)%>%tail(5)
