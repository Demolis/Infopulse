
library('dplyr')
library('tidyr')
library('ggplot2')

salaries_SF <- read.csv("Salaries.csv")

str(salaries_SF)
ssf <- salaries_SF

dc <- c('Id', 'Notes', 'Agency')
#ssf <- subset(ssf, select = -c(1, 11, 12))
ssf <- ssf %>% select( -one_of(dc))
# Names & Job Title
ssf_names <- select(ssf, c(1:2))

ssf_names %>% 
  mutate_all(funs(factor(.)))

# Pay and Benefits
ssf_pay <- select(ssf, c(3:8)) %>% 
  mutate_if(is.factor, as.character) %>% 
  mutate_if(is.character, as.double)

ssf <- bind_cols(ssf_names, ssf_pay, select(ssf, c(9:10)))
ssfreg<-ssf[1:500,]
#------------------------

out<-boxplot(ssfreg$TotalPay)
out
out$out

df_clean<-filter(df,!(CO2 %in% out$out))


ssfreg$TotalPay %in% out$out
boxplot(filter(ssfreg,!(TotalPay %in% out$out))$TotalPay)

install.packages("outliers")
library(outliers) 
grubbs.test(ssfreg$TotalPay, type = 10)


#-----------------------------------------------------------------------	
#   Neural Network
#-----------------------------------------------------------------------	


# Read the Data http://lib.stat.cmu.edu/DASL/Datafiles/Cereals.html
data = read.csv("cereals.csv", header=T)
head(data)
# Random sampling
samplesize = 0.60 * nrow(data)
set.seed(80)

index = sample( seq_len(nrow(data)), size = samplesize )


## Scale data for neural network
max = apply(data , 2 , max)
min = apply(data, 2 , min)
scaled = as.data.frame(scale(data, center = min, scale = max - min))
scaled
## Fit neural network 

# install library
install.packages("neuralnet", dep=T)

# load library
library(neuralnet)

# creating training and test set
trainNN = scaled[index , ]
testNN = scaled[-index , ]

# fit neural network
set.seed(2)   
f<-as.formula("rating ~ calories + protein + fat + sodium + fiber")
NN = neuralnet(f, trainNN, hidden = c(2,3),linear.output = T,rep=100)

which.min(NN$result.matrix[1,])
# plot neural network
plot(NN,rep=71)
## Prediction using neural network

predict_testNN = compute(NN, testNN[,c(1:5)],rep=71)
predict_testNN$net.result

X=(x-min)/(max-min)
X*(max-min)=x-min
x=X*(max-min)+min

predict_testNN = (predict_testNN$net.result * (max(data$rating) - min(data$rating))) + min(data$rating)
data[-index , 6]
plot(data[-index,]$rating, predict_testNN, col='blue', pch=16, ylab = "predicted rating NN", xlab = "real rating")
abline(0,1)
# Calculate Root Mean Square Error (RMSE)
RMSE.NN = (sum((data[-index,]$rating - predict_testNN)^2) / nrow(testNN)) ^ 0.5
RMSE.NN/median(data$rating)*100



#-------#
## Cross validation of neural network model


library(plyr)

# Initialize variables
set.seed(50)
k <- 10

List <- matrix(nrow = k,ncol=56)

# Fit neural network model within nested for loop
for(j in 10:65){
  for (i in 1:k) {
    index = sample(1:nrow(data),j )
    
    trainNN = scaled[index,]
    testNN = scaled[-index,]
    datatest = data[-index,]
    
    NN = neuralnet(rating ~ calories + protein + fat + sodium + fiber, trainNN, hidden = c(2,3), linear.output= T)
    predict_testNN = compute(NN,testNN[,c(1:5)])
    predict_testNN = (predict_testNN$net.result*(max(data$rating)-min(data$rating)))+min(data$rating)
    
    List [i,j-9]<- (sum((datatest$rating - predict_testNN)^2)/nrow(datatest))^0.5
  }

}
List
plot(apply(List,2,mean))

