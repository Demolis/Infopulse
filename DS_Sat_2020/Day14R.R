
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
