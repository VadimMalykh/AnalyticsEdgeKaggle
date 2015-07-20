# just try different models and features

library(randomForest)
library(caTools)
library(ROCR)
library(tm)
library(SnowballC)

train = read.csv("eBayiPadTrain.csv", stringsAsFactors = FALSE, fileEncoding="windows-1252")
test = read.csv("eBayiPadTest.csv", stringsAsFactors = FALSE, fileEncoding="windows-1252")

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

set.seed(123)
spl = sample.split(train$sold, SplitRatio = .8)
val = subset(train, spl == FALSE)
train = subset(train, spl == TRUE)

# simple glm models
lmodel1 = glm(sold~startprice, data=train, family=binomial)
lpred1 = predict(lmodel1, newdata=val, type="response")
ROCRpred = prediction(lpred1, val$sold)
ROCRperf = performance(ROCRpred, "auc")
ROCRperf # 0.7850872

lmodel2 = glm(sold~startprice+biddable, data=train, family=binomial)
lpred2 = predict(lmodel2, newdata=val, type="response")
ROCRpred = prediction(lpred2, val$sold)
ROCRperf = performance(ROCRpred, "auc")
ROCRperf # 0.8333866

lmodel3 = glm(sold~startprice+biddable+productline, data=train, family=binomial)
lpred3 = predict(lmodel3, newdata=val, type="response")
ROCRpred = prediction(lpred3, val$sold)
ROCRperf = performance(ROCRpred, "auc")
ROCRperf # 0.8516279

lmodel4 = glm(sold~startprice+biddable+productline+condition+new, data=train, family=binomial)
lpred4 = predict(lmodel4, newdata=val, type="response")
ROCRpred = prediction(lpred4, val$sold)
ROCRperf = performance(ROCRpred, "auc")
ROCRperf # 0.8522093

lmodel5 = glm(sold~startprice+biddable+productline+condition, data=train, family=binomial)
lpred5 = predict(lmodel5, newdata=val, type="response")
ROCRpred = prediction(lpred5, val$sold)
ROCRperf = performance(ROCRpred, "auc")
ROCRperf # 0.8522093

