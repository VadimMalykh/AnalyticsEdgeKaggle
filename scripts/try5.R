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
rm(train, test)

corpus = Corpus(VectorSource(data$description))
corpus = tm_map(corpus, tolower)
corpus = tm_map(corpus, PlainTextDocument)
corpus = tm_map(corpus, removePunctuation)
corpus = tm_map(corpus, removeWords, stopwords("english"))
corpus = tm_map(corpus, removeWords, c("ipad"))
corpus = tm_map(corpus, stemDocument)

frequencies = DocumentTermMatrix(corpus)

words = as.data.frame(as.matrix(frequencies))
colnames(words) = make.names(colnames(words))
data = cbind(data, words)

data$description = NULL
data$condition = as.factor(data$condition)
data$cellular = as.factor(data$cellular)
data$carrier = as.factor(data$carrier)
data$color = as.factor(data$color)
data$storage = as.factor(data$storage)
data$productline = as.factor(data$productline)

Train = subset(data, is.train == 1)
Test = subset(data, is.train == 0)

Train$is.train = NULL
Train$UniqueID = NULL

#remove zero columns from train
zeroColumns = colSums(Train[,10:ncol(Train)]) == 0
Train = Train[, c(rep(TRUE, 9), !zeroColumns)]
Test = Test[, c(rep(TRUE, 11), !zeroColumns)]

#PCA
wordTrain = Train[,10:ncol(Train)]
preProcVals = preProcess(wordTrain, method = "pca", thresh=.7)
pcaTrain = predict(preProcVals, wordTrain)
wordTest = Test[,12:ncol(Test)]
pcaTest = predict(preProcVals, wordTest)
Train = cbind(Train[,1:9], pcaTrain)
Test = cbind(Test[, 1:11], pcaTest)

tc = trainControl(method = "cv", number = 5)
#CV on RF
cv = train(sold~., data=Train, method="rf", trControl = tc)
#mtry  RMSE       Rsquared   RMSE SD      Rsquared SD
#2   0.4538179  0.2102904  0.002641092  0.02124389 
#106   0.3854193  0.4039957  0.017161774  0.05281506 
#210   0.3877369  0.3985129  0.019646020  0.05776977 

#CV on glm
cv = train(sold~., data=Train, method="glm", family=binomial, trControl = tc)
#RMSE      Rsquared   RMSE SD     Rsquared SD
#0.474636  0.2203433  0.05057359  0.05555082 

pred = predict(cv$finalModel, newdata=Test, type="response")

submission = data.frame(UniqueID = Test$UniqueID, Probability1 = pred)
write.csv(submission, "submission.csv", row.names = FALSE)
