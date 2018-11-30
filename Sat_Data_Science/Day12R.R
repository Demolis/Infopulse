
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
prp(model_cer)

fancyRpartPlot(model_cer)

pred<-predict(model_cer, newdata = test, type="vector")

sqrt(sum((pred - test$rating)^2))/length(pred)/mean(data$rating)

importance(model_cer)
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
  res[i] <- sqrt(sum((predictions - test$rating)^2))/length(predictions)/mean(data$rating)
  print(res[i])
}
plot(res,type="l")

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
predictions <- predict(rf, test)
print(sqrt(sum((as.integer(predictions) - as.integer(test$age))^2))/length(predictions))

importance(rf)





#===============================================

install.packages("plotly", dep=T)

library(plotly)
library(ggplot2)

df<-read.csv("DataDay4-1.csv", sep=";", dec = ",")


plot_ly(x=df$Par.1, y=df$Par.2)

subplot(
  plot_ly(x = 1:25, y = 1:25, symbol = I(1:25), name = "pch"),
  plot_ly(x=df$Par.1, y=df$Par.2,name = "test")
)


z<-plot_ly(x=df$Par.1, y=df$Par.2,z=c(1:nrow(df)),name = "test")

z

add_lines(z)



p <- plot_ly(diamonds, y = ~price, color = I("black"), 
             alpha = 0.1, boxpoints = "suspectedoutliers")
p
p%>% add_boxplot(x = "Overall")

#for mac - download https://www.xquartz.org
pic<-ggplot(df,aes(x=Par.1,y=Par.2))+geom_line()
pic
gg<-ggplotly(pic)
gg

#=======================================================

install.packages("shiny", dep=T)
library(shiny)


runApp("1")


https://shiny.rstudio.com/gallery/kmeans-example.html

https://shiny.rstudio.com/gallery/file-upload.html
#for visualization
https://plot.ly/r/shiny-tutorial/
  http://jkunst.com/highcharter/shiny.html  

