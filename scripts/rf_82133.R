# Random Forest
# test - 0.82133 
# val - 0.8425488
# train - 0.8633099

library(randomForest)
library(caTools)
library(ROCR)

train = read.csv("eBayiPadTrain.csv")
train$biddable = as.factor(train$biddable)
train$description = NULL
train$UniqueID = NULL
set.seed(123)
spl = sample.split(train$sold, SplitRatio = 0.75)
val = subset(train, spl == FALSE)
ttrain = subset(train, spl == TRUE)
rfModel = randomForest(sold~., data = ttrain)
rfPred = predict(rfModel, newdata = val)
ROCRpred = prediction(rfPred, val$sold)
ROCRperf = performance(ROCRpred, "auc")
ROCRperf

# generate submission
test = read.csv("eBayiPadTest.csv")
test$biddable = as.factor(test$biddable)
levels(test$productline) = levels(train$productline)
rfModel = randomForest(sold~., data = train)
rfPred = predict(rfModel, newdata = test)
submission = data.frame(UniqueID = test$UniqueID, Probability1 = rfPred)
write.csv(submission, "submission.csv", row.names = FALSE)

# check train auc
rfPred = predict(rfModel)
ROCRpred = prediction(rfPred, train$sold)
ROCRperf = performance(ROCRpred, "auc")
ROCRperf