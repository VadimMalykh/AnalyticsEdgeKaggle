# Random Forest with words
# test - ?
# val - ?
# train - 0.8646958

library(randomForest)
library(caTools)
library(ROCR)
library(tm)
library(SnowballC)
library(cluster)

train = read.csv("eBayiPadTrain.csv", stringsAsFactors = FALSE, fileEncoding="latin1")
test = read.csv("eBayiPadTest.csv", stringsAsFactors = FALSE, fileEncoding="latin1")

test$sold = NA
train$is.train = 1 
test$is.train = 0
data = rbind(train, test)
rm(train, test)

corpus = Corpus(VectorSource(data$description))
rm(for_corpus)
corpus = tm_map(corpus, tolower)
corpus = tm_map(corpus, PlainTextDocument)
corpus = tm_map(corpus, removePunctuation)
corpus = tm_map(corpus, removeWords, stopwords("english"))
corpus = tm_map(corpus, removeWords, c("ipad"))
corpus = tm_map(corpus, stemDocument)

frequencies = DocumentTermMatrix(corpus)
sparse = removeSparseTerms(frequencies, 0.99)
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

train = subset(data, is.train == 1)
test = subset(data, is.train == 0)

train$UniqueID = NULL
train$is.train = NULL

rfModel = randomForest(sold~., data = train)
rfPred = predict(rfModel, newdata = test)

submission = data.frame(UniqueID = test$UniqueID, Probability1 = rfPred)
write.csv(submission, "submission.csv", row.names = FALSE)

# check train auc
rfPred = predict(rfModel)
ROCRpred = prediction(rfPred, train$sold)
ROCRperf = performance(ROCRpred, "auc")
ROCRperf

