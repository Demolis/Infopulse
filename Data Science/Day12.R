install.packages("dplyr")
library(dplyr)
src_sqlite
testdb <- src_mysql(dbname = "classicmodels", 
                    host = "35.204.237.147",
                    user = "user",
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

mydb = dbConnect(MySQL(), user='user', password='Univer123', dbname='classicmodels', host='35.204.237.147')

dbListTables(mydb)

dbListFields(mydb, 'offices')

rs = dbSendQuery(mydb, "select * from offices")

data = fetch(rs, n=-1)

str(data)
