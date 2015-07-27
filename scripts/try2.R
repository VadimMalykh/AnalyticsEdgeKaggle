# Try
# test - ?
# train - ?

# I did features selection ;))
# Select only features with most significance (glm, summary), importance (rf, your.rf.model$importance), non-zero variance (nearZeroVar), low features-correlation (corrplot).
# Not too few (underfitting), not too many (overfitting), just optimal ;)

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
#rm(train, test)

corpus = Corpus(VectorSource(data$description))
corpus = tm_map(corpus, tolower)
corpus = tm_map(corpus, PlainTextDocument)
corpus = tm_map(corpus, removePunctuation)
corpus = tm_map(corpus, removeWords, stopwords("english"))
corpus = tm_map(corpus, removeWords, c("ipad"))
corpus = tm_map(corpus, stemDocument)

frequencies = DocumentTermMatrix(corpus)
sparse = removeSparseTerms(frequencies, 0.99)
#rm(corpus, frequencies)

words = as.data.frame(as.matrix(sparse))
colnames(words) = make.names(colnames(words))
data = cbind(data, words)
#rm(words, sparse)

# data$description = NULL
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

Train1 = subset(Train, nchar(description)==0)
Train2 = subset(Train, nchar(description)!=0)
Test1 = subset(Test, nchar(description)==0)
Test2 = subset(Test, nchar(description)!=0)

Train1 = Train1[,2:10]
lmodel = glm(sold~., data=Train1, family=binomial)

Train2$description = NULL

rfmodel = randomForest(sold~., data=Train2)
importantNames = rownames(subset(rfmodel$importance, rfmodel$importance>1))
Train2 = Train2[, c(importantNames, "sold")]
rfmodel = randomForest(sold~., data=Train2)

lpred = predict(lmodel, newdata = Test1, type="response")
rfpred = predict(rfmodel, newdata = Test2)

sub1 = data.frame(UniqueID = Test1$UniqueID, Probability1 = lpred)
sub2 = data.frame(UniqueID = Test2$UniqueID, Probability1 = rfpred)
submission = rbind(sub1, sub2)
write.csv(submission, "submission.csv", row.names = FALSE)
