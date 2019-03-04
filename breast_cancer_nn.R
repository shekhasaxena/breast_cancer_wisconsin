rm(list = ls())
setwd("/Users/shekhasaxena/Documents")

library(neuralnet)
library(nnet)
library(caret)

mydata <- read.csv("breast_cancer_dataset.csv")
mydata$diagnosis = ifelse( mydata$diagnosis=="M", 1, 0)

#removing unnecessary data columns and correlation models
mydata <- mydata[ , -c(1,5,6, 9, 10, 15, 16, 19, 20, 23:33)]

selected.var <- c(1:12)
# partition
set.seed(1)  
train.index <- sample(c(1:dim(mydata)[1]), dim(mydata)[1]*0.6)  
train <- mydata[train.index, ]
valid <- mydata[-train.index, ]

nn <- neuralnet(diagnosis ~ ., data = train, hidden = c(4,2), linear.output = FALSE)
plot(nn)

preds <- compute(nn, train[, -c(1)])
preds.class <- ifelse(preds$net.result>0.5,1,0)
confusionMatrix(as.factor(preds.class),as.factor(train$diagnosis))

preds.valid <- compute(nn, valid[,-c(1)])
preds.valid.class <- ifelse(preds.valid$net.result>0.5,1,0)
confusionMatrix(as.factor(preds.valid.class),as.factor(valid$diagnosis))
