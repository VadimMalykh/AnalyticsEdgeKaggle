# Clustering then glm
# test - 0.81474 (bad :()
# train - 0.8433677

library(tm)
library(SnowballC)
library(caTools)
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



model1 = glm(sold~startprice+biddable, data = train1, family = binomial)
model2 = glm(sold~startprice+biddable, data = train2, family = binomial)
model3 = glm(sold~startprice+biddable, data = train3, family = binomial)

pred1 = predict(model1, newdata = test1, type = "response")
pred2 = predict(model1, newdata = test2, type = "response")
pred3 = predict(model1, newdata = test3, type = "response")

test1$pred = pred1
test2$pred = pred2
test3$pred = pred3

test = rbind(test1, test2, test3)

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
