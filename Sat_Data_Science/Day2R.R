# Factor

gender<-c(0,1,0,0,0,0,1,1,1,1,0,1,0,1,0)
str(gender)

gender<-factor(gender, lev=c(0,1))
gender
levels(gender)<-c("M","F")
gender
class(gender)
as.numeric(gender)


fact<-0:20
cut(fact,breaks=4)

cut(fact,breaks=quantile(fact), labels = c("Q2","Q3","Q4"),include.lowest = T)

#functions
i<-{a<-2; b<-3; a; a+b}

summa <-function(a,b){
  return(a+b)
}
#source()
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

#Dplyr and tidyr
install.packages(c("dplyr","tidyr"), dep=T)
library(dplyr)
library(tidyr)
df<-read.csv("DataDay1.csv",sep=";",dec = ",")
str(df)
select(df,Hospital.beds)
select(df,-Hospital.beds)

df2<-df %>% 
  select(-(Hospital.beds:Population))

df %>% 
  select(starts_with("h")) %>%
  head
##
#starts_with()
#ends_with() 
#contains() 
##

df %>% 
  select(-(Hospital.beds:Population)) %>%
  filter(Conflicts.intencity %in% c(1,3)) %>%
  head

df %>% 
  select(-(Hospital.beds:Population)) %>%
  filter(Conflicts.intencity %in% c(1,3)) %>%
  arrange(Conflicts.intencity, desc(Country)) %>%
  head

df %>% 
  mutate(GDP = GDP.per.capita / Population) %>%
  head

df %>%
summarise(avg_gdp = mean(GDP.per.capita))

#sd(), min(), max(), median(), sum(), 
#n() (returns the length of vector), 
#first() (returns first value in vector), 
#last() (returns last value in vector)
df %>% group_by(Conflicts.intencity)%>%
  summarise(avg_gdp = mean(GDP.per.capita),kol=n())

head(df)
newdf<-df %>%
  gather(Variables,Value, Conflicts.intencity:Population)
View(newdf)

newdf%>%
  spread(Variables,Value)




df %>% 
  filter(4:5 > 5) %>%
  arrange(Conflicts.intencity, desc(Country)) %>%
  head
