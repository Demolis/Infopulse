#slide 5
#1
df<-read.csv("DataDay3.csv",sep=";",dec = ",")
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
