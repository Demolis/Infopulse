
data <- read.csv("cereals.csv", header=T)

head(data)
# Random sampling
samplesize = 0.60 * nrow(data)
set.seed(80)

index = sample( seq_len(nrow(data)), size = samplesize )

# creating training and test set
train <- data[index , ]
test <- data[-index , ]

#tree
install.packages("rpart.plot")
install.packages("rattle")

library(rpart)
library(rpart.plot)
library(rattle)
library(RColorBrewer)


model_cer<-rpart(rating ~ calories + protein + fat + sodium + fiber,
      data = train, method = "anova",
      control=rpart.control(minbucket = 2))
 model_cer
prp(model_cer,type=2)

fancyRpartPlot(model_cer)

pred<-predict(model_cer, newdata = test, type="vector")

sqrt(sum((pred - test$rating)^2)/length(pred))/median(data$rating)

max((pred - test$rating)/test$rating)

#the set of trees

ntrees <- 300
res <- numeric(ntrees)
prd <- numeric(nrow(test))
for(i in 1:ntrees){
  #выберем 80% рядов и колонок из множества данных
  x <- runif(nrow(train))>0.2;
  y <- runif(ncol(train))>0.2;
  #обязательно включим rating
  y[6] <- TRUE
  traindata <- train[x,y]
  #генерим полное дерево не оптимизируя процесс
  tree <- rpart(rating ~ ., traindata, control=rpart.control(cp=.0))
  #усредняем предсказание со всеми предыдущими деревьями
  prd <- prd + predict(tree, test)
  predictions <- prd / i
  #оцениваем ошибку
  res[i] <- sqrt(sum((predictions - test$rating)^2)/length(predictions))/mean(data$rating)
  print(res[i])
}
plot(res,type="l")
min(res)
#random forest
#только для категориальных переменных
library(randomForest)
data <- read.table("bloss.txt", header=T)
samplesize = 0.60 * nrow(data)
set.seed(80)

index = sample( seq_len(nrow(data)), size = samplesize )

# creating training and test set
train <- data[index , ]
test <- data[-index , ]

rf <- randomForest(age ~ ., train)
rf
predictions <- predict(rf, test)
sqrt(sum((as.integer(predictions) - as.integer(test$age))^2)/length(predictions))
sum(predictions==test$age)/length(predictions)
randomForest::importance(rf)


