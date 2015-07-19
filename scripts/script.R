# I need to make clusters using description, biddable, condition, cellurar, carrier, color, productline
# then I need logistic regression sold~startprice

# Other approach
# clusters using description, condition, carrier
# then tries sold~startprice + cellurar + color + storage + productline

train = read.csv("eBayiPadTrain.csv")
test = read.csv("eBayiPadTest.csv")
train$description = NULL

ValSet <- function(){
  library(caTools)
  spl = sample.split(train$sold, SplitRatio = .8)
  val = subset(train, spl == FALSE)
  train = subset(train, spl == TRUE)
}
