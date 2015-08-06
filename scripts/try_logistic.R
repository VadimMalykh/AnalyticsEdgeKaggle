# Try PCA + cv on randomFores and glm
# test - ?
# train - ?

Sys.setlocale("LC_ALL", "C")

library(tm)
library(SnowballC)
library(caTools)
library(ROCR)
library(randomForest)

train = read.csv("eBayiPadTrain.csv", stringsAsFactors = FALSE)
test = read.csv("eBayiPadTest.csv", stringsAsFactors = FALSE)

test$sold = NA
train$is.train = 1 
test$is.train = 0
data = rbind(train, test)

data$condition = as.factor(data$condition)
data$cellular = as.factor(data$cellular)
data$carrier = as.factor(data$carrier)
data$color = as.factor(data$color)
data$storage = as.factor(data$storage)
data$productline = as.factor(data$productline)

#feature engineering
data$ncond[data$condition == "For parts or not working"] = 3
data$ncond[data$condition == "Used"] = 2
data$ncond[is.na(data$ncond)] = 1

data$ncar[data$carrier == "Unknown"] = 1
data$ncar[is.na(data$ncar)] = 2

data$nstor[data$storage == "128"] = 1
data$nstor[is.na(data$nstor)] = 2

data$ncel[data$cellular == "Unknown"] = 1
data$ncel[is.na(data$ncel)] = 2

data$ncol[data$color == "Gold" | data$color == "White"] = 1
data$ncol[is.na(data$ncol)] = 2

Train = subset(data, is.train == 1)
Test = subset(data, is.train == 0)

tc = trainControl(method = "cv", number = 5)
#CV on glm
cv = train(sold~startprice+biddable+ncond+ncar+ncol+ncel+nstor, data=Train, method="glm", family=binomial, trControl = tc)
#RMSE       Rsquared   RMSE SD      Rsquared SD
#0.3976591  0.3635793  0.004935267  0.0143094  

pred = predict(cv$finalModel, newdata=Test, type="response")

submission = data.frame(UniqueID = Test$UniqueID, Probability1 = pred)
write.csv(submission, "submission.csv", row.names = FALSE)
