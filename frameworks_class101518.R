getwd()

setwd("C:/Users/casey/OneDrive/Documents/Frameworks I/")

wines <- read.csv("winequality-white.csv", sep = ";")

summary(wines)

library(caret)

set.seed(69)

train_index <- createDataPartition(y = wines$quality, p = 0.7, list = F)

train <- wines[train_index, ]
test <- wines[-train_index, ]

lm(quality ~ ., data = train)

