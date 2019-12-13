install.packages("ROCR")
library(ROCR)

data(ROCR.simple)
df <- data.frame(ROCR.simple)
pred <- prediction(df$predictions, df$labels)
perf <- performance(pred,"tpr","fpr")
plot(perf,colorize=TRUE)

library(PRROC)

cur <- roc.curve(scores.class0 = df$predictions, weights.class0=df$labels,
                       curve=TRUE)
plot(cur)
