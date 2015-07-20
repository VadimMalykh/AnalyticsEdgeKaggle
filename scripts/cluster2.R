# Clustering then trees
# test - 0.80289 (very very bad :()
# train - 0.8638135

library(tm)
library(SnowballC)
library(caTools)
library(cluster)
library(rpart)
library(rpart.plot)
library(ROCR)

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

#clustering by all features except startprice and sold
for_clusters = data
for_clusters$description = NULL
for_clusters$startprice = NULL
for_clusters$sold = NULL
for_clusters$biddable = NULL
for_clusters$UniqueID = NULL

distances = daisy(for_clusters)
hclusters = hclust(distances, method = "ward.D")
hgroups = cutree(hclusters, k = 3)
rm(hclusters, for_clusters, distances)


train1 = subset(data, hgroups == 1 & is.train == 1)
train2 = subset(data, hgroups == 2 & is.train == 1)
train3 = subset(data, hgroups == 3 & is.train == 1)

test1 = subset(data, hgroups == 1 & is.train == 0)
test2 = subset(data, hgroups == 2 & is.train == 0)
test3 = subset(data, hgroups == 3 & is.train == 0)


train1$UniqueID = NULL
train1$is.train = NULL
train2$UniqueID = NULL
train2$is.train = NULL
train3$UniqueID = NULL
train3$is.train = NULL

model1 = rpart(sold~., data = train1)
prp(model1)

model2 = rpart(sold~., data = train2)
prp(model2)

model3 = rpart(sold~., data = train3)
prp(model3)

pred1 = predict(model1, newdata = test1)
pred2 = predict(model2, newdata = test2)
pred3 = predict(model3, newdata = test3)

test1$pred = pred1
test2$pred = pred2
test3$pred = pred3

test = rbind(test1, test2, test3)

rm(test1, test2, test3)

submission = data.frame(UniqueID = test$UniqueID, Probability1 = test$pred)
write.csv(submission, "submission.csv", row.names = FALSE)

#check train auc
pred1 = predict(model1)
pred2 = predict(model2)
pred3 = predict(model3)
train1$pred = pred1
train2$pred = pred2
train3$pred = pred3
train = rbind(train1, train2, train3)
ROCRpred = prediction(train$pred, train$sold)
ROCRperf = performance(ROCRpred, "auc")
ROCRperf