#-----------------------------------------------------------------------	
#   Maps and Shape Files in R
#-----------------------------------------------------------------------	

library(maptools)

# Shape file:
Regions <- readShapePoly("Fra_adm1.shp")

slotNames(Regions)
Regions@data
str(Regions@polygons)
Regions@data$NAME_1

# Region of France:
spplot(Regions,
       "NAME_1", 
       scales = list(draw = T), 
       col.regions = rainbow(n = 22) ) 
library(RColorBrewer)
spplot(Regions,
       "NAME_1", 
       scales = list(draw = T), col.regions = brewer.pal(22, "Set3"),
       par.settings = list(axis.line = list(col = NA)))

# We want to add some data
# The order is important

Regions@data$Value = rnorm(22)
mypalette <- colorRampPalette(c("seagreen", "whitesmoke"))

# mypalette - is a function:
mypalette
spplot(Regions, "Value",
       col.regions = mypalette(20),  
       col = "transparent", # without borders
       par.settings = list(axis.line = list(col = NA)))

#the same in ggplot2
install.packages('rgeos',dep=T)
install.packages('rgdal', type='source')
library(ggplot2) 
library(rgeos)
library(maptools)

counties <- fortify(Regions, region = "NAME_1")
str(counties)
ggplot() + geom_map(data = counties,
                    aes(map_id = id), 
                    map = counties) + 
  expand_limits(x = counties$long, y = counties$lat) +
  coord_map("polyconic")

fake_data <- as.data.frame(Regions@data)
ggplot() + geom_map(data = fake_data, aes(map_id = NAME_1, fill = Value),                   
                    map = counties) + expand_limits(x = counties$long, y = counties$lat) + coord_map("polyconic")

# In other colors:
library(scales) 
ggplot() + geom_map(data = fake_data,
                    aes(map_id = NAME_1, fill = Value),
                    colour = "gray",
                    map = counties) + 
  expand_limits(x = counties$long, y = counties$lat) +
  scale_fill_gradient2(low = muted("blue"), 
                       midpoint = 0,
                       mid = "white",
                       high = muted("red"),
                       limits = c(min(fake_data$Value),
                                  max(fake_data$Value))) +
  coord_map("polyconic")


#-----------------------------------------------------------------------	
#   Neural Network
#-----------------------------------------------------------------------	


# Read the Data http://lib.stat.cmu.edu/DASL/Datafiles/Cereals.html
data = read.csv("cereals.csv", header=T)

# Random sampling
samplesize = 0.60 * nrow(data)
set.seed(80)
index = sample( seq_len ( nrow ( data ) ), size = samplesize )

#data <- data[, sapply(data, is.numeric)]

## Scale data for neural network
max = apply(data , 2 , max)
min = apply(data, 2 , min)
scaled = as.data.frame(scale(data, center = min, scale = max - min))

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
NN = neuralnet(rating ~ calories + protein + fat + sodium + fiber, trainNN, hidden = 3 , linear.output = T )

# plot neural network
plot(NN)

## Prediction using neural network

predict_testNN = compute(NN, testNN[,c(1:5)])
predict_testNN = (predict_testNN$net.result * (max(data$rating) - min(data$rating))) + min(data$rating)

plot(data[-index,]$rating, predict_testNN, col='blue', pch=16, ylab = "predicted rating NN", xlab = "real rating")

abline(0,1)
# Calculate Root Mean Square Error (RMSE)
RMSE.NN = (sum((data[-index,]$rating - predict_testNN)^2) / nrow(testNN)) ^ 0.5


## Cross validation of neural network model

# install relevant libraries
install.packages("boot")

# Load libraries
library(plyr)

# Initialize variables
set.seed(50)
k = 100
RMSE.NN = NULL

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
mean(List)
