# Try
# test - ?
# train - ?

Sys.setlocale("LC_ALL", "C")

library(tm)
library(SnowballC)
library(caTools)
library(ROCR)
library(randomForest)
library(caret)

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

nzv = nearZeroVar(Train)
Train = Train[, -nzv]

cv = train(sold~., data=Train, method="rf")
pred = predict(cv, newdata=Test)

#mtry  RMSE       Rsquared   RMSE SD      Rsquared SD
#2    0.4209302  0.3568421  0.004589823  0.02979545 
#21    0.3916149  0.3911336  0.009158193  0.02482269 
#40    0.3990768  0.3748473  0.009183898  0.02341234 


submission = data.frame(UniqueID = Test$UniqueID, Probability1 = pred)
write.csv(submission, "submission.csv", row.names = FALSE)
