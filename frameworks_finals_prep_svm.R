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

# Support Vector Machines

set.seed(0617)
data <- data.frame(x1 = rnorm(100), x2 = rnorm(100))
data$y <- factor(ifelse(data$x1 > data$x2, 0, 1))
data <- data[abs(data$x1-data$x2) > 0.2, ]

# Visualize data

ggplot(data,aes(x=x1,y=x2,color=y))+
  geom_point()+
  guides(color=F)

# Visualize data with hyperplane divider (linear classifier)

ggplot(data,aes(x=x1,y=x2,color=y))+
  geom_point()+
  guides(color=F)+
  geom_abline(slope = 1,intercept = 0,color='cadetblue', size=1)

# Visualize different Maximal Margin Classifiers

ggplot(data,aes(x=x1,y=x2,color=y))+
  geom_point()+
  guides(color=F)+
  geom_abline(slope = 1,intercept = 0,color='cadetblue', size=1)+
  geom_abline(slope = 1,intercept = -0.2,color='rosybrown', size=1)+
  geom_abline(slope = 1,intercept = 0.2,color='rosybrown', size=1)

# In real life, classes are rarely separable in this way

set.seed(0617)

data = data.frame(x1=rnorm(100),x2=rnorm(100))
data$y = factor(ifelse(data$x1>data$x2,0,1))
data$y[abs(data$x1-data$x2)<0.5] = factor(sample(c(0,1),size = length(data$y[abs(data$x1-data$x2)<0.5]),replace = T))

# Visualize non-separable data

ggplot(data,aes(x=x1,y=x2,color=y))+
  geom_point()

# Need to use a soft margin classifier

ggplot(data,aes(x=x1,y=x2,color=y))+
  geom_point()+
  guides(color=F)+
  geom_abline(slope = 1,intercept = 0,color='cadetblue', size=1)+
  geom_abline(slope = 1,intercept = -0.2,color='rosybrown', size=1)+
  geom_abline(slope = 1,intercept = 0.2,color='rosybrown', size=1)

# Another soft margin classifier with larger margins

ggplot(data,aes(x=x1,y=x2,color=y))+
  geom_point()+
  guides(color=F)+
  geom_abline(slope = 1,intercept = 0,color='cadetblue', size=1)+
  geom_abline(slope = 1,intercept = -0.4,color='violet', size=1)+
  geom_abline(slope = 1,intercept = 0.4,color='violet', size=1)

# Even larger margins

ggplot(data,aes(x=x1,y=x2,color=y))+
  geom_point()+
  guides(color=F)+
  geom_abline(slope = 1,intercept = 0,color='cadetblue', size=1)+
  geom_abline(slope = 1,intercept = -0.8,color='purple', size=1)+
  geom_abline(slope = 1,intercept = 0.8,color='purple', size=1)

# As cost goes up, margins get slimmer (lower cost = bigger margins)
# Need to tune to optimal parameter

ggplot(data,aes(x=x1,y=x2,color=y))+
  geom_point()+
  guides(color=F)+
  geom_abline(slope = 1,intercept = 0,color='cadetblue', size=1)+
  geom_abline(slope = 1,intercept = -0.2,color='rosybrown', size=1)+
  geom_abline(slope = 1,intercept = 0.2,color='rosybrown', size=1)+
  geom_abline(slope = 1,intercept = -0.4,color='violet', size=1)+
  geom_abline(slope = 1,intercept = 0.4,color='violet', size=1)+
  geom_abline(slope = 1,intercept = -0.8,color='purple', size=1)+
  geom_abline(slope = 1,intercept = 0.8,color='purple', size=1)

# SVM Models

set.seed(0617)

data = data.frame(x1=rnorm(100),x2=rnorm(100))
data$y = factor(ifelse(data$x1>data$x2,0,1))

set.seed(0617)
split = sample(1:nrow(data),0.7*nrow(data))
train = data[split,]
test = data[-split,]

# Cost = 1

library(e1071)

svmLinear = svm(y ~ .,
                train,
                kernel='linear',
                scale=F,
                type='C-classification')
# if outcome is a factor, default type='C-classification'

summary(svmLinear)

# Plot decision boundary

beta = t(svmLinear$coefs) %*% svmLinear$SV

slope = -beta[1]/beta[2]

intercept = svmLinear$rho/beta[2]

ggplot(train,aes(x=x1,y=x2,color=y))+
  geom_point()+
  guides(color=F)+
  geom_abline(slope = slope,intercept = intercept,color='cadetblue', size=1)

# Plot with margins

ggplot(train,aes(x=x1,y=x2,color=y))+
  geom_point()+
  guides(color=F)+
  geom_abline(slope = slope,intercept = intercept,color='cadetblue', size=1)+
  geom_abline(slope = slope,intercept = intercept-1/beta[2],color='rosybrown', size=1)+  
  geom_abline(slope = slope,intercept = intercept+1/beta[2],color='rosybrown', size=1)

plot(svmLinear, train) # default plot with svm

# examine performance of model

pred <- predict(svmLinear)

table(pred, train$y)

# examine performance on test data

pred = predict(svmLinear,newdata=test)

table(pred,test$y)

# cost = 100

svmLinear = svm(y ~ .,
                train, 
                kernel = 'linear',
                scale = F,
                type = 'C-classification',
                cost=100) # if outcome is a factor, default type='C-classification'

beta = t(svmLinear$coefs) %*% svmLinear$SV

slope = -beta[1]/beta[2]

intercept = svmLinear$rho/beta[2]

ggplot(train,aes(x=x1,y=x2,color=y))+
  geom_point()+
  guides(color=F)+
  geom_abline(slope = slope,intercept = intercept,color='cadetblue', size=1)

summary(svmLinear)

# higher cost = narrower margins

ggplot(train,aes(x=x1,y=x2,color=y))+
  geom_point()+
  guides(color=F)+
  geom_abline(slope = slope,intercept = intercept,color='cadetblue', size=1)+
  geom_abline(slope = slope,intercept = intercept-1/beta[2],color='rosybrown', size=1)+  
  geom_abline(slope = slope,intercept = intercept+1/beta[2],color='rosybrown', size=1)

# examine performance of model

pred = predict(svmLinear)
table(pred, train$y)

# examine performance on test data

pred = predict(svmLinear,newdata=test)
table(pred,test$y)

# Tune model for best cost parameter

svmTune = tune(method = svm,
               y ~ .,
               data = train,
               kernel = 'linear', 
               type = 'C-classification', 
               scale = F, 
               ranges = list(cost=c(0.01,0.1,1, 10, 100)))

svmTune$best.model

# examine performance of model

pred = predict(svmTune$best.model,newdata=test)
table(pred,test$y)

plot(svmTune$best.model,test) # plot of best model

# Polynomial Support Vector Machines

# look at situation where linear SVM is unlikely to succeed

set.seed(0617)
data = data.frame(x1=runif(200,-1,1),x2=runif(200,-1,1))
radius = .8
radius_squared = radius^2
data$y <- factor(ifelse(data$x1^2+data$x2^2<radius_squared, 0, 1))
split = sample(1:nrow(data),0.7*nrow(data))
train = data[split,]
test = data[-split,]

ggplot(train,aes(x=x1,y=x2,color=y))+
  geom_point()+
  guides(color=F)+
  geom_abline(slope = -1,intercept = 0.64,color='cadetblue', size=1)

svmLinear = svm(y ~ .,
                data = train, 
                kernel = 'linear',
                scale = F,
                type = 'C-classification')

pred = predict(svmLinear)
mean(pred==train$y)

plot(svmLinear,train)

# New SVM with Polynomial Kernel

svmPoly = svm(y ~ .,
              data = train, 
              kernel='polynomial',
              scale=F,
              type='C-classification',
              degree=2)

# examine model performance

pred = predict(svmPoly)
mean(pred==train$y)

# examine model on test data

pred = predict(svmPoly,newdata=test)
mean(pred==test$y)

plot(svmPoly, train)

# Tune model

tune_svmPoly = tune(method = svm,
                    y ~ .,
                    data = train,
                    kernel='polynomial',
                    ranges= list(degree=c(2,3), 
                                 cost = c(0.01, 0.1, 1), 
                                 gamma=c(0,1,10), 
                                 coef0=c(0,0.1,1,10)))

summary(tune_svmPoly)

# Find best model

tune_svmPoly$best.model

# examine performance

pred = predict(tune_svmPoly$best.model)
mean(pred==train$y)

# examine on test data

pred = predict(tune_svmPoly$best.model,newdata=test)
mean(pred==test$y)

plot(tune_svmPoly$best.model,train)

# SVMs - Radial Basis Function
# In practice, the Radial Basis function performs better than 
# either Linear or Polynomial kernels as it can fit a variety of decision boundaries.

svmRadial = svm(y ~ .,
                data = train, 
                kernel='radial',
                scale=F,
                type='C-classification')

pred = predict(svmRadial)
mean(pred==train$y)

# tune radial model

tune_svmRadial = tune(method = 'svm',
                      y ~ .,
                      data = train,
                      kernel = 'radial', 
                      type='C-classification',
                      ranges = list(cost=c(0.1,10,100), 
                                    gamma=c(1,10), 
                                    coef0 = c(0.1,1,10)))

summary(tune_svmRadial$best.model)

# evaluate best radial model

pred = predict(tune_svmRadial$best.model)
mean(pred==train$y)

# examine on test data

pred = predict(tune_svmRadial$best.model,newdata=test)
mean(pred==test$y)

plot(tune_svmRadial$best.model,test)

# Illustration with wine data set

data <- read.csv("winequality-white.csv",sep=";")

data$quality = factor(ifelse(data$quality>mean(data$quality), 1, 0),
                      labels = c('high','low'))

set.seed(1706)

split = sample.split(data$quality,SplitRatio = 0.7)
train = data[split,]
test = data[!split,]

# compare performance of trees and SVMs

tree = rpart(quality ~ alcohol+volatile.acidity,
             train,
             method='class')

pred = predict(tree, newdata=test, type = 'class')

table(pred, test$quality)

mean(pred==test$quality)

# Linear SVM

svmLinear = svm(quality ~ alcohol+volatile.acidity, 
                data = train,
                kernel = 'linear',
                type = 'C-classification')

summary(svmLinear)

pred = predict(svmLinear, newdata=test)

table(pred,test$quality)

mean(pred==test$quality)

plot(svmLinear,test[,c('quality','alcohol','volatile.acidity')]) 
# decision boundary looks non-linear because original variables were scaled

# Linear SVM - Tuned

svmLinearTune = tune(method = svm,
                     quality ~ alcohol+volatile.acidity,
                     data = train,
                     kernel = 'linear',
                     type = 'C-classification',
                     ranges = list(cost=c(0.01, 0.1,1,10,100)))

summary(svmLinearTune$best.model)

pred = predict(svmLinearTune$best.model, newdata = test)

table(pred,test$quality)

mean(pred==test$quality) # same as first SVM model

plot(svmLinearTune$best.model,test[,c('quality','alcohol','volatile.acidity')])
# same as previous plot

# Polynomial SVM

svmPolynomial = svm(quality ~ alcohol+volatile.acidity,
                    data = train,
                    kernel='polynomial',
                    degree = 2,
                    type = 'C-classification')

summary(svmPolynomial)

pred = predict(svmPolynomial, newdata = test)
table(pred, test$quality)

mean(pred==test$quality) # less accurate than linear SVM

plot(svmPolynomial,test[,c('quality','alcohol','volatile.acidity')])

# Polynomial SVM - tuned

svmPolynomialTune = tune(method = svm,
                         quality ~ alcohol+volatile.acidity,
                         data = train,
                         kernel = 'polynomial',
                         ranges=list(cost=c(0.01,1,100),
                                     degree=c(2,3)))

summary(svmPolynomialTune$best.model)

svmPolynomialTune$best.parameters

pred = predict(svmPolynomialTune$best.model, newdata = test)

table(pred,test$quality)

mean(pred==test$quality)

plot(svmPolynomialTune$best.model,test[,c('quality','alcohol','volatile.acidity')])

# Radial SVM

svmRadial = svm(quality~alcohol+volatile.acidity,
                data = train,
                kernel='radial',
                type='C-classification')

summary(svmRadial)

pred = predict(svmRadial, newdata = test)

table(pred, test$quality)

mean(pred==test$quality)

plot(svmRadial,test[,c('quality','alcohol','volatile.acidity')])

# SVM Radial - tuned

svmRadialTune = tune(method = svm,
                     quality ~ alcohol+volatile.acidity,
                     data = train,
                     kernel = 'radial',
                     type = 'C-classification',
                     ranges = list(cost=c(0.1,10,100), 
                                   gamma=c(1,10), 
                                   coef0 = c(0.1,1,10)))

summary(svmRadialTune$best.model)

svmRadialTune$best.parameters

pred = predict(svmRadialTune$best.model, newdata = test)

table(pred, test$quality)

mean(pred==test$quality)

plot(svmRadialTune$best.model,test[,c('quality','alcohol','volatile.acidity')])


