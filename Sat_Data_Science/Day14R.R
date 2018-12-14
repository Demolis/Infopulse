library(dplyr)

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
library(RMySQL)

mydb = dbConnect(MySQL(), user='test', password='Univer123', dbname='classicmodels', host='77.47.193.46')

dbListTables(mydb)

dbListFields(mydb, 'orderdetails')

rs = dbSendQuery(mydb, "select * from orders")

data = fetch(rs, n=-1)

str(data)
