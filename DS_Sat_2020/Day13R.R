library(mice)
df<-read.csv("Day6-titanic.csv")
summary(df)
newdf<-df[,c(2,3,5,6,7,8,10)]

newdf<-complete(mice(newdf,1,method = "rf"),1)
newdf$Sex<-as.numeric(newdf$Sex)
princdf<-princomp(scale(newdf))
summary(princdf)
plot(princdf)
princdf
princdf$loadings
pred<-predict(princdf)

plot(pred[,1:2],type="n")
text(pred[,1:2],labels=df$Survived)

biplot(princdf)

#------
fdf<-factanal(newdf,3,scores = "reg")
fdf$scores
fdf
?biplot(fdf$loadings)

plot(fdf$scores[,1:2],type="n")
text(fdf$scores[,1:2],labels=df$Survived)



save(NN, file="NN.RData")
load(file="NN.RData")

#---------


install.packages("plotly", dep=T)

library(plotly)
library(ggplot2)

df<-read.csv("Day4-1.csv", sep=";", dec = ",")


plot_ly(x=df$Par.1, y=df$Par.2)

subplot(
  plot_ly(x = 1:25, y = 1:25, symbol = I(1:25), name = "pch"),
  plot_ly(x=df$Par.1, y=df$Par.2,name = "test")
)


z<-plot_ly(x=df$Par.1, y=df$Par.2,z=c(1:nrow(df)),name = "test")

z

add_lines(z)



p <- plot_ly(diamonds, y = ~price, color = I("black"), 
             alpha = 0.5, boxpoints = "suspectedoutliers")
p
p%>% add_boxplot(x = "Overall")

#for mac - download https://www.xquartz.org
pic<-ggplot(df,aes(x=Par.1,y=Par.2))+geom_line()
pic
gg<-ggplotly(pic)
gg

#=======================================================

install.packages("shiny", dep=T)
library(shiny)


runApp("1")


https://shiny.rstudio.com/gallery/kmeans-example.html

https://shiny.rstudio.com/gallery/file-upload.html
#for visualization
https://plot.ly/r/shiny-tutorial/
  http://jkunst.com/highcharter/shiny.html  