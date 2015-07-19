# Cross validation
# test - ?
# val - ?

#doesn't work well! ^(

library(caTools)
library(ROCR)
library(caret)
library(e1071)
library(rpart)
library(rpart.plot)

train = read.csv("eBayiPadTrain.csv")
train$biddable = as.factor(train$biddable)
train$description = NULL
train$UniqueID = NULL
set.seed(123)
spl = sample.split(train$sold, SplitRatio = 0.75)
val = subset(train, spl == FALSE)
ttrain = subset(train, spl == TRUE)
ttrain$sold = as.factor(ttrain$sold)
tc = trainControl(method = "cv", number = 10)
cpGrid = expand.grid(.cp = seq(.01, .99, .01))
train(sold~., data = ttrain, method = "rpart", trControl = tc, tuneGrid = cpGrid)
cp = 0.5
ttrain$sold = as.numeric(ttrain$sold)
treeModel = rpart(sold~., data = ttrain, cp = cp)
prp(treeModel)