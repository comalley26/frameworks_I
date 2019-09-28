setwd("C:/Users/casey/OneDrive/Documents/Frameworks I")

library(dplyr)
library(ggplot2)
library(tidyverse)
library(broom)
library(caret)
library(rpart)
library(stringr)
library(gbm)
library(caTools)
library(ISLR)
library(ROCR)
library(rattle)

head(OJ)
summary(OJ)
dim(OJ)

# Part I
# Q1: split train and test data

set.seed(1234)

oj_train_index <- sample.split(Y = OJ$Purchase, SplitRatio = .7, group = NULL)

oj_train <- OJ[oj_train_index, ]
oj_test <- OJ[-oj_train_index, ]

dim(oj_train) # 749 rows

# Q2: how many Minute Maid in training set?

oj_train %>%
  filter(Purchase == "MM") %>%
  summarise(n()) # 292

# Q3: average price for minute maid

oj_train %>%
  summarise(mean(PriceMM)) # 2.087223
oj_train

# Q4: average discount for minute maid

oj_train %>%
  summarise(mean(DiscMM)) # 0.1237116

# Q5: number of Minute Maid purchases made in week 275

oj_train %>%
  filter(WeekofPurchase == 275, Purchase == "MM") %>%
  summarise(n()) # 17

# Part II
# Q1: AUC for test sample

oj_model <- rpart(Purchase ~ PriceCH+PriceMM+DiscCH+DiscMM+
                             SpecialCH+SpecialMM+LoyalCH+
                             PriceDiff+PctDiscMM+PctDiscCH,
                  data = oj_train)

summary(oj_model)

fancyRpartPlot(oj_model)

pred = predict(oj_model, newdata = oj_test)

ROCRpred = prediction(pred[,2], oj_test$Purchase)

perf <- performance(ROCRpred, measure = "auc")

roc_ROCR <- performance(ROCRpred, measure = "tpr", x.measure = "fpr")
plot(roc_ROCR, main = "ROC curve", colorize = T)
abline(a = 0, b = 1)

auc_ROCR <- performance(ROCRpred, measure = "auc")
auc_ROCR <- auc_ROCR@y.values[[1]]
auc_ROCR # 0.8933847

# Q2: find optimal complexity for ROCR curve

set.seed(100)




