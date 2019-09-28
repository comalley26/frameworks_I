# Finals Prep for Frameworks I

setwd("C:/Users/casey/OneDrive/Documents/Frameworks I")

library(dplyr)
library(ggplot2)
library(tidyr)
library(tidyverse)
library(broom)
library(caret)
library(rpart)
library(stringr)
library(gbm)
library(randomForest)
library(xgboost)
library(ranger)
library(lubridate)
library(mice)
library(caTools)

ebay <- read.csv("eBayAssignment.csv")

str(ebay)

set.seed(100)

train_index <- sample.split(ebay$sold, .7)

train <- ebay[train_index, ]
test <- ebay[!train_index, ]

str(train)
str(test)

library(rpart)
library(rpart.plot)

# Regression Tree

tree1 <- rpart(sold ~ startprice, data=train) # default method='anova' used

rpart.plot(tree1)

summary(tree1)

# Classification Tree

tree2 <- rpart(sold ~ startprice, data = train, method='class')

rpart.plot(tree2)

summary(tree2)

# Increase tree complexity

tree1Complex = rpart(sold ~ startprice, data=train, cp=0.005) 
# default method='anova' used

rpart.plot(tree1Complex)

summary(tree1Complex)

# Tree using Storage as predictor

tree3 <- rpart(sold ~ storage, data = train, method='anova')

rpart.plot(tree3)

summary(tree3)

tree4 <- rpart(sold ~ storage, data = train, method='class')

rpart.plot(tree4)

# Tree with more variables

tree5 = rpart(sold~startprice+biddable+condition+cellular+carrier+color+
              storage+productline+noDescription+upperCaseDescription+
              startprice_99end,
              data = train,
              method = 'class')

rpart.plot(tree5)

summary(tree5)

pred5 = predict(tree5, newdata = test, type = 'class')
ct = table(test$sold, pred5); ct

accuracy = sum(diag(ct))/sum(ct); accuracy

specific = ct[1,1]/sum(ct[1,1],ct[1,2]); specific

sensitive = ct[2,2]/sum(ct[2,1],ct[2,2]); sensitive

# More complex tree

tree5Complex = rpart(sold ~ startprice+biddable+condition+cellular+
                     carrier+color+storage+productline+noDescription+
                     upperCaseDescription+startprice_99end,
                     data=train,
                     method='class',
                     control=rpart.control(minbucket = 25))

rpart.plot(tree5Complex) # no difference from less complex tree

# Need to change CP (complexity) to accomplish this and get rid of minbucket arg

tree5Complex = rpart(sold ~ startprice+biddable+condition+cellular+
                     carrier+color+storage+productline+noDescription+
                     upperCaseDescription+startprice_99end,
                     data = train,
                     method = 'class',
                     #control = rpart.control(minbucket = 25),
                     cp = 0.004) 

rpart.plot(tree5Complex)

summary(tree5Complex)

# Make predictions

pred = predict(tree5Complex, newdata = test, type='class')
ct = table(test$sold,pred); ct

accuracy = sum(diag(ct))/sum(ct); accuracy

specific = ct[1,1]/sum(ct[1,1],ct[1,2]); specific

sensitive = ct[2,2]/sum(ct[2,1],ct[2,2]); sensitive

# Regression tree

tree6 = rpart(sold~startprice+biddable+condition+cellular+carrier+color+
              storage+productline+noDescription+upperCaseDescription+
              startprice_99end,
              data = train)

rpart.plot(tree6)

pred = predict(tree6, newdata = test)

# ROC Curve

library(ROCR)

ROCRpred = prediction(pred, test$sold)
as.numeric(performance(ROCRpred,"auc")@y.values) # auc measure

plot(ROCRperf,xlab="1 - Specificity",ylab="Sensitivity") # relabeled axes

plot(ROCRperf,colorize=TRUE) # color coded ROC curve

plot(ROCRperf,colorize=TRUE,print.cutoffs.at=seq(0,1,0.2),text.adj=c(-0.3,2)) 
# color coded and annotated ROC curve



