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

#-----------------------------------------------------
  
ggplot(ssf,aes(x=TotalPay))+geom_histogram()+facet_wrap(~Year)

varpie <- select(ssfreg,2) %>%
  group_by(JobTitle) %>%
  summarise(number = n()) %>% arrange(number)

tail(varpie)

  pie(tail(varpie$number), main="Jobs", labels =tail(varpie$JobTitle),col= c(1,2,3,4,5,6),cex=0.6)
  legend("bottomright",legend= tail(varpie$JobTitle), cex=0.3, fill= c(1,2,3,4,5,6))
#------------------------------------------------------------
    dt<-tail(varpie)
    pie(dt$number, main="Jobs", labels =paste(round(dt$number*100/sum(dt$number),1),"%",sep=" "),col= c(1,2,3,4,5,6),cex=0.6)
  legend("bottomright",legend= tail(varpie$JobTitle), cex=0.6, fill= c(1,2,3,4,5,6))  
  
#------------------------------------------------------------ 
  bp<-ggplot(dt, aes(x=" ", y=number, fill=JobTitle))+ geom_bar(width = 1, stat = "identity")
  bp
  pie <- bp + coord_polar("y", start=0)
  pie  
  
  pie + scale_fill_manual(values=c("#999999", "#E69F00", "#56B4E9","#563241","#124356","#897653"))
  
  pie + scale_fill_brewer(palette="Blues")+theme_minimal()+ theme(axis.text.x=element_blank())
  
  
#------------------------------------------- 
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
  cor(sleep, use="pairwise.complete.obs")


  
  imp <- mice(sleep, seed=1234) #method – parameter of method
  imp$imp
  imp$m
  res <- complete(imp, action=2) 
  res  
  md.pattern(res) 
#---------------------------------------
  library(zoo)
  na.approx(sleep,na.rm=F)
  na.spline(sleep,na.rm=F)

  
  

  
  