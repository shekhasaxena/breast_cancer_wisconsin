rm(list = ls())
setwd("/Users/shekhasaxena/Documents")
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

library(randomForest)
rf <- randomForest(as.factor(diagnosis) ~ ., data = train, ntree = 500, 
                   mtry = 4, nodesize = 5, importance = TRUE)
install.packages("ggtree")
library(devtools)
devtools::install_github('araastat/reprtree')
library(reprtree)
tree <- getTree(rf, k=1, labelVar=TRUE)
#realtree <- reprtree:::as.tree(tree, rf)

summary(rf)
summary(tree)
head(rf$votes,10)

## Plot forest by prediction errors
plot(rf, type = "simple")

## variable importance plot
varImpPlot(rf, type = 1)


## confusion matrix
rf.pred <- predict(rf, valid)
library(caret)
confusionMatrix(rf.pred, as.factor(valid$diagnosis))
