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
NN = neuralnet(f, trainNN, hidden = c(2,3))

# plot neural network
plot(NN)

## Prediction using neural network

predict_testNN = compute(NN, testNN[,c(1:5)])

predict_testNN = (predict_testNN$net.result * (max(data$rating) - min(data$rating))) + min(data$rating)

plot(data[-index,]$rating, predict_testNN, col='blue', pch=16, ylab = "predicted rating NN", xlab = "real rating")

abline(0,1)
# Calculate Root Mean Square Error (RMSE)
RMSE.NN = (sum((data[-index,]$rating - predict_testNN)^2) / nrow(testNN)) ^ 0.5
RMSE.NN/median(data$rating)*100


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

