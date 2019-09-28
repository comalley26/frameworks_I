install.packages("ISLR")
library(ISLR)
install.packages("caTools")
library(caTools)
library(ggplot2)
library(caret)
library(dplyr)

str(OJ)
str(mpg)

# question 6

smp_size <- floor(0.8 * nrow(mpg))

set.seed(100)

train_ind <- sample(nrow(mpg), size = smp_size)

train <- mpg[train_ind,]
test <- mpg[-train_ind,]

mean(train$hwy) - mean(test$hwy)

# question 7
set.seed(100)

strat_train_ind <- createDataPartition(mpg$hwy, p = 0.8, list = FALSE, groups = 20)

strat_train <- mpg[strat_train_ind,]
strat_test <- mpg[-strat_train_ind,]

mean(strat_train$hwy) - mean(strat_test$hwy)

# question 8
?sample.split
dim(OJ)

train_ind_OJ <- sample.split(OJ$Purchase, SplitRatio = 3/5)

oj_train <- OJ[train_ind_OJ,]
oj_test <- OJ[-train_ind_OJ,]

sum(as.numeric(oj_train$Purchase == "MM"))



