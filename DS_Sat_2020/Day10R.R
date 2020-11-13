df<-read.csv("Spain_fruits.csv", sep=";", dec = ",")

str(df)
cor(df[,-1], use="pairwise.complete.obs")

summary(lm("Apples.Fuji ~ Apples.Golden", df))
summary(lm("Pears ~ Mandarins", df))


install.packages("plotly")

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