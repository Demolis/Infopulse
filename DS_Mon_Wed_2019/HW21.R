#1
test<-read.csv("Day21-pca.csv",sep=";",dec=",",stringsAsFactors = F)
#2
test$index_name<-NULL
test$better_value<-NULL
library(tidyr)
library(zoo)
test<-test[-c(65564,65565),]
testDF<-spread(test,index_abbr,val)
summary(testDF)
sum(is.na(testDF))
apply(testDF,2,function(x) sum(is.na(x)))/nrow(testDF)
for(i in c(unique(testDF$name_reg))){
  for(j in 4:ncol(testDF)){
    if (sum(is.na(testDF[testDF$name_reg==i,j]))!=length(testDF[testDF$name_reg==i,j])){
    testDF[testDF$name_reg==i,j]<-na.approx(testDF[testDF$name_reg==i,j],na.rm=F)
    testDF[testDF$name_reg==i,j]<-na.spline(testDF[testDF$name_reg==i,j])
    }
  }
}
sum(is.na(testDF))
apply(testDF,2,function(x) sum(is.na(x)))/nrow(testDF)

testDF[,-c(1,2,3)]<-complete(mice(testDF[,-c(1,2,3)],1,method = "rf"),1)
sum(is.na(testDF))

#3
library(nortest)
ntest<-c(0,0,0)
for (i in 4:ncol(testDF))
ntest<-c(ntest,ad.test(testDF[,i])$p.value)

hist(testDF[,which.max(ntest)],breaks=20)

#4
fact<-princomp(scale(testDF[,-c(1,2,3)]),scores=T)
fact
summary(fact) #we should chose 15 components
loadings(fact)[abs(loadings(fact)[,1])>0.13,1]
#Cql          Csl          GLC          INR           Is 
#0.1396245257 0.1379441821 0.1345063805 0.1447986444 0.1347971308 
#Isd          ITU         NISD          SLI          TLP 
#0.1382390677 0.1340464453 0.1352277090 0.1310347284 0.1343849421
#Mostly the parameters, which describe general situation
loadings(fact)[abs(loadings(fact)[,2])>0.14,2]
#AGV          EGS   ES_CoalRBP   ES_HBOther     ES_Hydro 
#0.1679058771 0.1514052997 0.1501140014 0.1522059451 0.1482638290 
#ES_Wind          ESB          GWK          HEX          HFC 
#0.1498925363 0.1497324924 0.1850157031 0.1502318217 0.1566971263 
#IGS          INV          PAP          RLL          SD4 
#0.1550668676 0.1771138681 0.1553718733 0.1562363613 0.1660582941 
#SRIN2        SRSC3          TCG          TTR 
#0.1501568052 0.1582692719 0.1499826082 0.1462802396 
#The parameters describes energetics and economics (or its influence)

#5
sum(testDF[,1]==2017)
clust1<-kmeans(testDF[testDF[,1]==2017,-c(1,2,3)],5,nstart=40)
clust2<-kmeans(fact$scores[testDF[,1]==2017,1:15],5,nstart=40)
clust<-cbind(testDF[testDF[,1]==2017,2:3],clust1$cluster,clust2$cluster)


hcmap("custom/world-lowres", data = clust, value = "clust1$cluster",
      joinBy = c("iso-a3", "abbr_reg"), name = "Clusterisation",
      dataLabels = list(enabled = TRUE, format = '{point.name}'),
      borderColor = "#FAFAFA", borderWidth = 0.1,
      tooltip = list(valueDecimals = 0, valuePrefix = "", valueSuffix = " cluster")) %>%
  hc_mapNavigation(enabled = TRUE) %>%
  hc_colorAxis(stops=list_parse2(data.frame(q = 1:5/5,
                                            c = substring(viridis(as.integer(5)), 0, 7),
                                            stringsAsFactors = FALSE))) 


hcmap("custom/world-lowres", data = clust, value = "clust2$cluster",
      joinBy = c("iso-a3", "abbr_reg"), name = "Clusterisation",
      dataLabels = list(enabled = TRUE, format = '{point.name}'),
      borderColor = "#FAFAFA", borderWidth = 0.1,
      tooltip = list(valueDecimals = 0, valuePrefix = "", valueSuffix = " cluster")) %>%
  hc_mapNavigation(enabled = TRUE) %>%
  hc_colorAxis(stops=list_parse2(data.frame(q = 1:5/5,
                                            c = substring(viridis(as.integer(5)), 0, 7),
                                            stringsAsFactors = FALSE))) 
#6
plot(fact$scores[testDF[,1]==2017,1],fact$scores[testDF[,1]==2017,2])
#tha anomality 
testDF[testDF[,1]==2017&fact$scores[testDF[,1]==2017,1]>13,2]
testDF[testDF[,1]==2017&fact$scores[testDF[,1]==2017,1]>20,2]
