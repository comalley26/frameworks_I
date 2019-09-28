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
library(lubridate)
library(mice)
library(caTools)
library(ISLR)

# Advanced Trees

# Cross Validation with S&P data (from Stocks HTML file)
# Exploration

str(Smarket)
summary(Smarket)

# Split into train and test (2005 is test year)

train = Smarket[Smarket$Year<=2004,]
test = Smarket[Smarket$Year==2005,]

# Visualize relationships using gpairs, corrplot, and ggplot2

library(gpairs)
gpairs(Smarket)

library(corrplot)
correlation_matrix = cor(Smarket[,c(2:8)])
corrplot(correlation_matrix)

ggplot(data=train,aes(x=Volume,color=factor(Year)))+
  geom_freqpoly(size=2)

ggplot(data=train,aes(x=Volume,color=factor(Year)))+
  geom_density(size=2)

# Logit Model

logModel1 = glm(Direction~Lag1+Lag2+Lag3+Lag4+Lag5+Volume,
                data=train,
                family="binomial")

summary(logModel1)

# Accuracy of model

predLog1 <- predict(logModel1, type = "response")

ct <- table(train$Direction, predLog1 > 0.5); ct

sum(diag(ct))/nrow(train) 

# Logit Model 2

logModel2 = glm(Direction~Lag1,
                data=train,
                family="binomial")

summary(logModel2)

predLog2 <- predict(logModel2, type="response")

ct <- table(train$Direction, predLog2 > 0.5); ct

sum(diag(ct))/nrow(train) 

# Assess best logit model on test data

predLogTest <- predict(logModel2, newdata = test, type = "response")

ct <- table(test$Direction,predLogTest>0.5); ct

sum(diag(ct))/nrow(test)

# Tree model

tree1 = rpart(Direction~Lag1+Lag2+Lag3+Lag4+Lag5+Volume,
              data=train,
              method="class")

prp(tree1) # Visualize tree

predTree1 <- predict(tree1, type="class")

ct <- table(train$Direction,predTree1); ct

sum(diag(ct))/nrow(train)

# Reduce tree complexity with minbucket = 25

tree2 = rpart(Direction~Lag1+Lag2+Lag3+Lag4+Lag5+Volume,
              data=train,
              method="class",
              control=rpart.control(minbucket=25))

prp(tree2)

predTree2 = predict(tree2,type="class")

ct = table(train$Direction,predTree2); ct

sum(diag(ct))/nrow(train)

# Reduce tree complexity even more with minbucket = 40

tree3 = rpart(Direction~Lag1+Lag2+Lag3+Lag4+Lag5+Volume,
              data=train,
              method="class",
              control=rpart.control(minbucket=40))

prp(tree3)

predTree3 = predict(tree3,type="class")

ct = table(train$Direction,predTree3); ct

sum(diag(ct))/nrow(train)

# Assess best tree on test data

predTree3_test = predict(tree3, newdata = test, type = "class")

ct = table(test$Direction,predTree3_test); ct

sum(diag(ct))/nrow(test)

# 10 fold cross validation 

trControl = trainControl(method="cv", number = 10) # 10-fold cross validation

tuneGrid = expand.grid(.cp=seq(0,0.1,0.001))  

set.seed(100)

trainCV = train(Direction~Lag1+Lag2+Lag3+Lag4+Lag5+Volume,data=train,
                method="rpart", trControl=trControl,tuneGrid=tuneGrid)

head(trainCV$results) # first few cv results

plot(trainCV)

trainCV$bestTune # best complexity parameter (CP)

# Apply tree model with optimal complexity to test data

treeCV = rpart(Direction~Lag1+Lag2+Lag3+Lag4+Lag5+Volume,
               data = train,
               method = "class", 
               control = rpart.control(cp = trainCV$bestTune))

predCV = predict(treeCV, newdata = test, type = "class")

ct = table(test$Direction,predCV); ct

sum(diag(ct))/nrow(test)

# Advanced Trees - Wages HTML File

data <- read.csv("wages_adv_trees.csv")

str(data)

# Create train and test sets

set.seed(1011)
split = sample(1:nrow(data),250)
train = data[split,]
test = data[-split,]

# Default decision tree
library(rpart.plot)
library(rpart)
        
tree <- rpart(earn ~ .,
              data = train)

predTree <- predict(tree, newdata = test)

rmseTree <- sqrt(mean((predTree - test$earn)^2)); rmseTree

# Maximal tree

maximalTree <- rpart(earn ~ .,
                     data = train, 
                     control = rpart.control(minbucket=1))

predMaximalTree <- predict(maximalTree, newdata = test)

rmseMaximalTree <- sqrt(mean((predMaximalTree - test$earn)^2)); rmseMaximalTree

# 10 fold cross validation of Tree

trControl <- trainControl(method="cv", number = 10)

tuneGrid <- expand.grid(.cp = seq(0.001, 0.1, 0.001))

set.seed(100)

cvModel <- train(earn~.,
                 data=train,
                 method="rpart",
                 trControl = trControl,
                 tuneGrid = tuneGrid)

cvModel$bestTune

treeCV <- rpart(earn ~ .,
                data = train,
                control = rpart.control(cp = cvModel$bestTune))

predTreeCV <- predict(treeCV, newdata = test)

rmseCV <- sqrt(mean((predTreeCV-test$earn)^2)); rmseCV

# Bootstrapping Models
# Bagging using Random Forest (specified by mtry argument)

set.seed(100)

bag <- randomForest(earn ~ .,
                    data=train,
                    mtry = ncol(train)-1,
                    ntree = 1000)

predBag <- predict(bag, newdata = test)

rmseBag <- sqrt(mean((predBag-test$earn)^2)); rmseBag

plot(bag)

varImpPlot(bag)
class(bag)

randomForest::importance(bag) # need to specify rf bc theres another in ranger

getTree(bag, k=100) # view tree 100

hist(treesize(bag))  # size of trees constructed 

# Random Forest Model

set.seed(100)

forest <- randomForest(earn ~ .,
                       data = train, 
                       ntree = 1000)

predForest <- predict(forest, newdata = test)

rmseForest <- sqrt(mean((predForest-test$earn)^2)); rmseForest

names(forest)
summary(forest)
plot(forest)

varImpPlot(forest); randomForest::importance(forest)  ## see variable importance

getTree(forest, k = 100)

hist(treesize(forest))

# Random Forest with 10-fold Cross Validation

trControl=trainControl(method = "cv", number = 10)

tuneGrid = expand.grid(mtry=1:5)

set.seed(100)

cvForest = train(earn~.,data=train,
                 method="rf",ntree=1000, 
                 trControl=trControl,
                 tuneGrid=tuneGrid )

cvForest  # best mtry was 2

set.seed(100)

forest = randomForest(earn ~ .,
                      data = train,
                      ntree = 100,
                      mtry = 2)

predForest = predict(forest,newdata=test)

rmseForest = sqrt(mean((predForest-test$earn)^2)); rmseForest

# Many boosting models available including
# gbm: Boosting with trees
# mboost: Model based boosting
# ada: statistical boosting based on additive logistic regression
# gamBoost: boosting GAM

set.seed(100)

boost <- gbm(earn ~ .,
             data=train,
             distribution="gaussian",
             n.trees = 100000,
             interaction.depth = 3,
             shrinkage = 0.001)

predBoostTrain <- predict(boost,n.trees = 100000)

rmseBoostTrain <- sqrt(mean((predBoostTrain-train$earn)^2)); rmseBoostTrain

summary(boost)

predBoost <- predict(boost, newdata=test, n.trees = 10000)

rmseBoost <- sqrt(mean((predBoost-test$earn)^2)); rmseBoost

# Boosting with cross-validation

set.seed(100)

trControl <- trainControl(method = "cv", number=10)

tuneGrid <- expand.grid(n.trees = 1000, 
                        interaction.depth = c(1,2),
                        shrinkage = (1:100)*0.001,
                        n.minobsinnode=5)
cvBoost = train(earn ~ .,
                data=train,
                method="gbm", 
                trControl=trControl, 
                tuneGrid=tuneGrid)

boostCV <- gbm(earn ~ .,
               data = train,
               distribution = "gaussian",
               n.trees = cvBoost$bestTune$n.trees,
               interaction.depth = cvBoost$bestTune$interaction.depth,
               shrinkage = cvBoost$bestTune$shrinkage,
               n.minobsinnode = cvBoost$bestTune$n.minobsinnode)

predBoostCV <- predict(boostCV, test, n.trees = 1000)

rmseBoostCV <- sqrt(mean((predBoostCV-test$earn)^2)); rmseBoostCV


