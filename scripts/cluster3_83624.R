# Clustering 
# test - 0.83624 !! (best for now!)
# train - ?

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

TrainNew = subset(Train, condition == "New" | new > 0)
set.seed(123)
spl = sample.split(TrainNew$sold, SplitRatio=.8)
train = subset(TrainNew, spl == TRUE)
val = subset(TrainNew, spl == FALSE)
val[val$productline=="iPad 2",]$productline = "Unknown"

model = glm(sold~startprice+biddable+storage, data=train, family=binomial)
pred = predict(model, newdata=val, type="response")
ROCRpred = prediction(pred, val$sold)
ROCRperf = performance(ROCRpred, "auc")
ROCRperf

TrainOther = subset(Train, condition != "New" & new == 0)
set.seed(123)
spl = sample.split(TrainOther$sold, SplitRatio=.8)
train = subset(TrainOther, spl == TRUE)
val = subset(TrainOther, spl == FALSE)

model = randomForest(sold~., data=train)
pred = predict(model, newdata=val)
ROCRpred = prediction(pred, val$sold)
ROCRperf = performance(ROCRpred, "auc")
ROCRperf


# train and predict on full data
TestNew = subset(Test, condition=="New" | new>0)
TestOther = subset(Test, condition!="New" & new==0)
lmodel = glm(sold~startprice+biddable+storage, data=TrainNew, family=binomial)
lpred = predict(lmodel, newdata=TestNew, type="response")
rfmodel = randomForest(sold~., data=TrainOther)
rfpred = predict(rfmodel, newdata=TestOther)

sub1 = data.frame(UniqueID = TestNew$UniqueID, Probability1 = lpred)
sub2 = data.frame(UniqueID = TestOther$UniqueID, Probability1 = rfpred)
submission = rbind(sub1, sub2)
write.csv(submission, "submission.csv", row.names = FALSE)
