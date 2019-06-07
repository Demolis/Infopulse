library(dplyr)
library(RMySQL)
#slide 8
db <- src_mysql(dbname = "classicmodels", 
                    host = "77.47.193.46",
                    user = "test",
                    port = 3306,
                    password = "Univer123")

src_tbls(db)
#task 1


orderdetails <-tbl(db,"orderdetails")


sel<-orderdetails %>% filter(priceEach>40) %>% 
  group_by(productCode)%>%summarise(Val=sum(quantityOrdered*priceEach))
sel

#task 2
orders <-tbl(db,"orders")

march_ord<-orders %>% filter(orderDate %like% "%-03-%") 

res<-as.data.frame(march_ord)


#task 3
products <-tbl(db,"products")
sel<-orderdetails %>% 
  group_by(productCode)%>%summarise(Val=sum(quantityOrdered))%>%left_join(select(products,productCode,productName),by="productCode")%>%
  arrange(desc(Val))%>%head(20)
df<-as.data.frame(sel)
df
library(ggplot2)
ggplot(aes(x=productName, y=Val),data=df)+geom_point()+theme(axis.text.x = element_text(angle = 90, hjust = 1))

#task 4
customers <-tbl(db,"customers")
sel <- orders %>% group_by(customerNumber)%>%summarise(num=n())%>%
  left_join(select(customers,customerNumber,creditLimit),by="customerNumber")
df<-as.data.frame(sel)
df
ggplot(aes(x=num, y=creditLimit),data=df)+geom_point()
summary(lm(df$creditLimit~df$num))
