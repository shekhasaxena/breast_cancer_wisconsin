rm(list = ls())
setwd("/Users/shekhasaxena/Documents")

library(DMwR)
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

# Find optimal K
set.seed(502)
grid1 <- expand.grid(.k = seq(2, 20, by = 1))
control <- trainControl(method = "cv")
knn.train <- train(diagnosis ~ ., data = train,
                   method = "knn",
                   trControl = control,
                   tuneGrid = grid1)
knn.train

knn.pred <- predict(knn.train, newdata = valid)
confusionMatrix(factor(knn.pred, levels = 0:1), factor(valid$diagnosis, levels = 0:1))


# Different distance weighting
#install.packages("kknn")
library(kknn)
set.seed(123)
kknn.train <- train.kknn(diagnosis ~ ., data = train, kmax = 25, 
                         distance = 2, 
                         kernel = c("rectangular", "triangular", "epanechnikov"))
plot(kknn.train)

kknn.train

kknn.pred <- predict(kknn.train, newdata = valid)
confusionMatrix(factor(kknn.pred, levels = 0:1), factor(valid$diagnosis, levels = 0:1))

