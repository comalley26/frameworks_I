getwd()
setwd("C:/Users/casey/OneDrive/Documents/Frameworks I")

ebay <- read.csv("eBayAssignment.csv")
names(ebay)

library(caTools)
set.seed(100)
split = sample.split(ebay$sold,SplitRatio = 0.7)
ebay_train = ebay[split,]
ebay_test = ebay[!split,]

#install.packages('rpart.plot')
#install.packages('rattle')
library(rpart)
library(rpart.plot)
library(rattle)

tree1 <- rpart(sold~startprice,data=ebay_train)
summary(tree1)

fancyRpartPlot(tree1)

ebay_pred1 <- predict(tree1, newdata = ebay_test)

mean((round(ebay_pred1) - ebay_test$sold)^2)

tree2 <- rpart(sold~startprice,data=ebay_train, method = "class")
summary(tree2)

fancyRpartPlot(tree2)
rpart.plot(tree2)

tree3 <- rpart(sold~startprice,data=ebay_train, cp = 0.005)
summary(tree3)
fancyRpartPlot(tree3)







