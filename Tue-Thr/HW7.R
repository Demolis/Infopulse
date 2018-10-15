#HW7
library(dplyr)
library(tidyr)
library(ggplot2)
library(outliers) 

#2
df<-read.csv("simpsons_episodes.csv")

#3
fam<-df%>%group_by(season,number_in_season)%>%summarise(m=mean(imdb_rating))%>%arrange(season,desc(m)) 
for (i in 28){
  print(i)
  t<-fam[fam$season=i,2]
  print(t[1])
}
#4
plot(df$id,df$views)
plot(df$id,df$imdb_rating)
plot(df$id,df$us_viewers_in_millions)
#5
tsV<-ts(df$us_viewers_in_millions)
plot(predict(arima(tsV,order=c(3,1,2)),h=10))
#6
shapiro.test(df$imdb_rating)
shapiro.test(df$imdb_votes)
#7
cor(df[,c(8,10,11)],use="pairwise.complete.obs")