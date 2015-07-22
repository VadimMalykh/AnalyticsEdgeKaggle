# Try
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
sparse = removeSparseTerms(frequencies, 0.985)
rm(corpus, frequencies)

words = as.data.frame(as.matrix(sparse))
colnames(words) = make.names(colnames(words))
data = cbind(data, words)
rm(words, sparse)

data$description = NULL
data$condition = as.factor(data$condition)
data$cellular = as.factor(data$cellular)
data$carrier = as.factor(data$carrier)
data$color = as.factor(data$color)
data$storage = as.factor(data$storage)
data$productline = as.factor(data$productline)

# trying
Train = subset(data, is.train == 1)
Test = subset(data, is.train == 0)

Train$is.train = NULL
Train$UniqueID = NULL

train = data
train$UniqueID = NULL
train$is.train = NULL
# just check some model on full data

set.seed(123)
spl = sample.split(Train$sold, SplitRatio=.75)
train = subset(Train, spl == TRUE)
val = subset(Train, spl == FALSE)

model = randomForest(sold~., data=train)
pred = predict(model, newdata=val)
ROCRpred = prediction(pred, val$sold)
ROCRperf = performance(ROCRpred, "auc")
ROCRperf


model = randomForest(sold~., data=Train)
pred = predict(model, newdata=Test)

submission = data.frame(UniqueID = Test$UniqueID, Probability1 = pred)
write.csv(submission, "submission.csv", row.names = FALSE)
