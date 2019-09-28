# Redoing assignments

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

# Assignment 6

houses <- read.csv("houses.csv")

set.seed(1031)

train_index <- createDataPartition(houses$price, p = 0.7, groups = 100, list = F)

train <- houses[train_index, ]
test <- houses[-train_index, ]

mean(train$price) # 540674.2

mod1 <- lm(price ~ ., data = train)

cor(train$bedrooms, train$price) # 0.3147962
cor(train$bathrooms, train$price) # 0.5304317
cor(train$sqft_lot, train$price) # 0.09052413
cor(train$condition, train$price) # 0.03790087 (lowest)
cor(train$grade, train$price) # 0.6620404

library(corrplot)
corrplot(cor(train[,c(3:7, 10:13,16)]),method = 'square',type = 'lower',diag = F)
# sqft_living and bathrooms has biggest square out of 4 options

cor(train$sqft_living, train$sqft_above + train$sqft_basement) # 1

mod2 <- lm(price ~ bedrooms+bathrooms+sqft_living+sqft_lot+floors+
                   waterfront+view+condition+grade+age,
           data = train)

library(car)
vif(mod2) # sqft_living has largest VIF

library(leaps)
subsets <- regsubsets(price ~ ., data = train, nvmax=6 )
summary(subsets) # only ones with stars next to 6 (bedrooms, sqft_living) are included

best6 <- lm(price ~ bedrooms + sqft_living + waterfront + view + grade + age,
            data = train)

summary(best6) # r-squared is 0.6441

# Forward Stepwise

start_mod <- lm(price ~ 1, data = train)
empty_mod <- lm(price ~ 1, data = train)
full_mod <- lm(price ~ bedrooms+bathrooms+sqft_living+sqft_lot+floors+
                       waterfront+view+condition+grade+age, 
               data = train)

forwardStepwise <-step(start_mod,
                       scope=list(upper=full_mod,lower=empty_mod),
                       direction = 'forward')

summary(forwardStepwise) # all possible choices included


# Backward Stepwise

start_mod2 = lm(price ~ bedrooms+bathrooms+sqft_living+sqft_lot+floors+
                        waterfront+view+condition+grade+age, 
                data = train)
empty_mod2 = lm(price ~ 1, data = train)
full_mod2 = lm(price ~ bedrooms+bathrooms+sqft_living+sqft_lot+floors+
                       waterfront+view+condition+grade+age, 
               data = train)

backwardStepwise <- step(start_mod2,
                         scope=list(upper=full_mod2, lower=empty_mod2),
                         direction = 'backward')

summary(backwardStepwise) # all possible choices included

# Hybrid Stepwise

start_mod3 = lm(price ~ 1, data = train)
empty_mod3 = lm(price ~ 1, data = train)
full_mod3 = lm(price ~ bedrooms+bathrooms+sqft_living+sqft_lot+floors+
                 waterfront+view+condition+grade+age, 
               data = train)

hybridStepwise <-step(start_mod3,
                      scope=list(upper=full_mod3,lower=empty_mod3),
                      direction='both')

summary(hybridStepwise) # all possible choices included

# Question 6
# Ridge Regression

library(glmnet)

x = model.matrix(price ~ bedrooms+bathrooms+sqft_living+sqft_lot+floors+
                   waterfront+view+condition+grade+age - 1, 
                 data = train)
y = train$price

ridgeModel = glmnet(x,y,alpha=0)
ridgeModel

plot(ridgeModel,xvar='lambda',label=T)

cv.ridge = cv.glmnet(x,y,alpha=0) # default is 10-fold cross validation
plot(cv.ridge)

# Lasso Regression

lassoModel = glmnet(x,y, alpha=1) 
# Note default for alpha is 1 which corresponds to Lasso

lassoModel

plot(lassoModel,xvar='lambda',label=T)

plot(lassoModel,xvar='dev',label=T)

cv.lasso = cv.glmnet(x,y,alpha=1) # 10-fold cross-validation
plot(cv.lasso)

coef(cv.lasso) # variables are bathrooms, sqft_living, waterfront, view, grade, age

lasso_mod <- lm(price ~ bathrooms + sqft_living + waterfront + view + grade + age,
                 data = train)

summary(lasso_mod) # r-squared is 0.6435

# Dimension Reduction 

trainPredictors = train[,c(3:11,16)]
testPredictors = test[,c(3:11,16)]
x = preProcess(x = trainPredictors,method = 'pca',thresh = 0.9)
trainComponents = predict(x,newdata=trainPredictors)
trainComponents$price = train$price
  
dim_model <- lm(price ~ ., trainComponents) # 7 predictor variables

summary(dim_model)$r.squared # r-squared is 0.5482695

testComponents = predict(x,newdata=testPredictors)
testComponents$price = test$price

test_dim_preds <- predict(dim_model, newdata = testComponents)

rss <- sum((test_dim_preds - test$price)^2)
tss <- sum((mean(test$price) - test$price)^2)
1 - rss/tss # test r-squared is .559





