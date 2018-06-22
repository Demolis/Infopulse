install.packages("plotly", dep=T)
library(plotly)

df<-read.csv("Day6-1.csv", sep=";", dec = ",")


plot_ly(x=df$Par.1, y=df$Par.2)

subplot(
  plot_ly(x = 1:25, y = 1:25, symbol = I(1:25), name = "pch"),
  plot_ly(x=df$Par.1, y=df$Par.2,name = "test")
)


z<-plot_ly(x=df$Par.1, y=df$Par.2,z=c(1:nrow(df)),name = "test")

z

add_lines(z)



p <- plot_ly(diamonds, y = ~price, color = I("black"), 
             alpha = 0.1, boxpoints = "suspectedoutliers")
p
p%>% add_boxplot(x = "Overall")


#=======================================================

install.packages("shiny", dep=T)
library(shiny)


runApp("Shiny/1")


runApp("Shiny/2")







