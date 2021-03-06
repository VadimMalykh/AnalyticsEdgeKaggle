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

words = as.data.frame(as.matrix(frequencies))
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

#remove columns with zero sums
Train = Train[, c(rep(TRUE, 9), colSums(Train[10:ncol(Train)])!=0)]

Train[, c(-3,-4,-5,-6,-7,-8,-9)] = scale(Train[, c(-3,-4,-5,-6,-7,-8,-9)])

# train full RF to get impornance
#model = randomForest(sold~., data=Train)
#importants = subset(model$importance, model$importance>.5)
#Train = Train[, c(rownames(importants), "sold")]

set.seed(123)
spl = sample.split(Train$sold, SplitRatio=.75)
train = subset(Train, spl == TRUE)
val = subset(Train, spl == FALSE)

lmodel = glm(sold~biddable+startprice+condition+cellular+storage+productline+X100+alway+box+broken+hous+littl+may+slight+still+tear, data=train, family=binomial)
lpred = predict(lmodel, newdata=val, type="response")
ROCRpred = prediction(lpred, val$sold)
ROCRperf = performance(ROCRpred, "auc")
ROCRperf


lmodel = glm(sold~biddable+startprice+condition+cellular+storage+productline+X100+alway+box+broken+hous+littl+may+slight+still+tear, data=Train, family=binomial)
lpred = predict(lmodel, newdata=Test, type="response")

submission = data.frame(UniqueID = Test$UniqueID, Probability1 = lpred)
write.csv(submission, "submission.csv", row.names = FALSE)
