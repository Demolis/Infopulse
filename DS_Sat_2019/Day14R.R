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






#===============================================
library(dplyr)
library(RPostgreSQL)


testdb <- src_postgres(dbname = "dbinfopulse", 
                    host = "77.47.193.41",
                    user = "infopulse",
                    port = 5432,
                    password = "infoivan")


src_tbls(testdb)

customer <-tbl(testdb,"customer")
store <-tbl(testdb,"store")

cust<-customer %>% filter(store_id==2) %>% 
  select(first_name,last_name,email)
show_query(cust)
cust <- as.data.frame(cust)



mydb = dbConnect(PostgreSQL(), user='infopulse', password='infoivan', dbname='dbinfopulse', host='77.47.193.41')

dbListTables(mydb)

dbListFields(mydb, 'film')

rs = dbSendQuery(mydb, "select * from country")

data = fetch(rs, n=-1)

str(data)

dbDisconnect(mydb)
