library(mice)
df<-read.csv("Day6-titanic.csv",stringsAsFactors = T)
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

plot(fdf$scores[,1:2],type="n")
text(fdf$scores[,1:2],labels=df$Survived)


#-------

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
