library(mice)
df<-read.csv("Day9-titanic.csv")
summary(df)
newdf<-df[,c(2,3,5,6,7,8,10)]

newdf<-complete(mice(newdf,1,method = "rf"),1)
newdf$Sex<-as.numeric(newdf$Sex)


princdf<-princomp(scale(newdf))

summary(princdf)
plot(princdf)

princdf$loadings
pred<-predict(princdf)

plot(pred[,1:2],type="n")
text(pred[,1:2],labels=df$Survived)

biplot(princdf)

#------
fdf<-factanal(newdf,3,scores = "reg")
fdf$scores

fdf

plot(fdf$scores[,1:2],type="n")
text(fdf$scores[,1:2],labels=df$Survived)
