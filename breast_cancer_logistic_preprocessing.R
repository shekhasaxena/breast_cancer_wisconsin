rm(list = ls())
setwd("/Users/shekhasaxena/Documents")
mydata <- read.csv("breast_cancer_dataset.csv")

#install.packages("GGally")
#for ggcorr - to plot correlation
library(GGally)

#check missing values
colSums(is.na(mydata))

#removing unnecessary data columns
mydata <- mydata[ , -c(1, 33)]

head(mydata)

#changing to binary
mydata$diagnosis = ifelse( mydata$diagnosis=="M", 1, 0)

#plot diagnosis count
table(mydata$diagnosis)
barplot(table(mydata$diagnosis), main="Diagnosis", 
        xlab="0 - Benign, 1 - Malign", density= 569)

#check multicollinearity
ggcorr(mydata[, -1],
       label = TRUE,
       label_alpha = TRUE)

cor(mydata[, -1])

#removing all worst columns, perimeter and area
mydata <- mydata[ , -c(4,5,14,15,22:31)]

#check multicollinearity
ggcorr(mydata[, -1],
       label = TRUE,
       label_alpha = TRUE)

cor(mydata[, -1])

#removing concavity and concave points columns
mydata <- mydata[ , -c(6,7,14,15)]

#check multicollinearity
ggcorr(mydata[, -1],
       label = TRUE,
       label_alpha = TRUE)

cor(mydata[, -1])

selected.var <- c(1:13)
#sample
set.seed(2)
train.index <- sample(c(1:dim(mydata)[1]), dim(mydata)[1]*0.6)
train <- mydata[train.index, selected.var]
valid <- mydata[-train.index, selected.var]

logit.reg <- glm(diagnosis ~ ., data = train, family = "binomial") 
options(scipen=999) # remove scientific notation
summary(logit.reg)

logit.reg.pred <- predict(logit.reg, valid, type = "response")
library(caret)
confusionMatrix(as.factor(ifelse(logit.reg.pred > 0.5, 1, 0)), as.factor(valid$diagnosis))

# Variable Selection
# Forward
null<-glm(diagnosis ~ 1, data = train, family = "binomial") 
step(null, scope=list(lower=null, upper=logit.reg), direction="both")

#removing symmetry_mean, smoothness_se and compactness_se columns
mydata <- mydata[ , -c(11)]

#check multicollinearity
ggcorr(mydata[, -1],
       label = TRUE,
       label_alpha = TRUE)

cor(mydata[, -1])

selected.var <- c(1:12)
#sample
set.seed(2)
train.index <- sample(c(1:dim(mydata)[1]), dim(mydata)[1]*0.6)
train <- mydata[train.index, selected.var]
valid <- mydata[-train.index, selected.var]

logit.reg <- glm(diagnosis ~ ., data = train, family = "binomial") 
options(scipen=999) # remove scientific notation
summary(logit.reg)

logit.reg.pred <- predict(logit.reg, valid, type = "response")
library(caret)
confusionMatrix(as.factor(ifelse(logit.reg.pred > 0.5, 1, 0)), as.factor(valid$diagnosis))