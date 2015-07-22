# Clustering 
# test - 0.77760 (baddly bad! :((((
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

TrainOther = subset(Train, condition %in% c("For parts or not working", "Used") | condit>0 | use>0 | scratch>0 | cosmet>0 | minor>0)
set.seed(123)
spl = sample.split(TrainOther$sold, SplitRatio=.8)
train = subset(TrainOther, spl == TRUE)
val = subset(TrainOther, spl == FALSE)
#val[val$productline=="iPad 2",]$productline = "Unknown"

model = randomForest(sold~startprice+biddable+productline+storage+color+carrier+perfect+great+good+crack, data=train)
pred = predict(model, newdata=val)
ROCRpred = prediction(pred, val$sold)
ROCRperf = performance(ROCRpred, "auc")
ROCRperf

TrainNew = subset(Train, !(condition %in% c("For parts or not working", "Used") | condit>0 | use>0 | scratch>0 | cosmet>0 | minor>0))
set.seed(123)
spl = sample.split(TrainNew$sold, SplitRatio=.8)
train = subset(TrainNew, spl == TRUE)
val = subset(TrainNew, spl == FALSE)

model = glm(sold~startprice+biddable+storage+productline, data=train, family=binomial)
pred = predict(model, newdata=val, type="response")
ROCRpred = prediction(pred, val$sold)
ROCRperf = performance(ROCRpred, "auc")
ROCRperf


# train and predict on full data
TestNew = subset(Test, !(condition %in% c("For parts or not working", "Used") | condit>0 | use>0 | scratch>0 | cosmet>0 | minor>0))
TestOther = subset(Test, condition %in% c("For parts or not working", "Used") | condit>0 | use>0 | scratch>0 | cosmet>0 | minor>0)
rfmodel = randomForest(sold~startprice+biddable+productline+storage+color+carrier+perfect+great+good+crack, data=TrainOther)
rfpred = predict(model, newdata=TestOther)
lmodel = glm(sold~startprice+biddable+storage+productline, data=TrainNew, family=binomial)
lpred = predict(model, newdata=TestNew, type="response")


sub1 = data.frame(UniqueID = TestNew$UniqueID, Probability1 = lpred)
sub2 = data.frame(UniqueID = TestOther$UniqueID, Probability1 = rfpred)
submission = rbind(sub1, sub2)
write.csv(submission, "submission.csv", row.names = FALSE)
