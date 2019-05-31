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
             alpha = 0.1, boxpoints = "suspectedoutliers")
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






#===============================================
library(dplyr)
library(RMySQL)

testdb <- src_mysql(dbname = "classicmodels", 
                    host = "77.47.193.46",
                    user = "test",
                    port = 3306,
                    password = "Univer123")


src_tbls(testdb)

orderdetails <-tbl(testdb,"orderdetails")
orders <-tbl(testdb,"orders")

sel<-orderdetails %>% filter(priceEach>40) %>% 
  select(orderNumber,quantityOrdered,priceEach)

show_query(sel)


install.packages("RMySQL", dep=T)

mydb = dbConnect(MySQL(), user='test', password='Univer123', dbname='classicmodels', host='77.47.193.46')

dbListTables(mydb)

dbListFields(mydb, 'orderdetails')

rs = dbSendQuery(mydb, "select * from orders")

data = fetch(rs, n=-1)

str(data)
