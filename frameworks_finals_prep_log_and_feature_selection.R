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

# logistic regression review from eBay HTML file
# set up training and testing data sets

ebay <- read.csv("eBayAssignment.csv")

str(ebay)

set.seed(100)

train_index <- sample.split(ebay$sold, .7)

train <- ebay[train_index, ]
test <- ebay[!train_index, ]

str(train)
str(test)

# exploration through visualizations

ggplot(data=train,aes(x=productline,y=sold,fill=productline))+
  geom_bar(stat='summary',fun.y='mean')+
  coord_flip()

ggplot(data=train,aes(x=startprice_99end,y=sold,fill=startprice_99end))+
  geom_bar(stat='summary',fun.y='mean')

# first logistic regression model (only based on start price)

model1 <- glm(sold ~ startprice, data=train, family='binomial')

summary(model1)

summary(model1)$coef[2] # coefficient for startprice

# percent increase in likelihood an iPad being sold with a $1 increase in price

100*(exp(summary(model1)$coef[2])-1)

# probability of iPad selling at $200

predict(model1, newdata=data.frame(startprice=200), type='response')

# Model 2 based on storage

model2 <- glm(sold~storage, data=train, family='binomial')
summary(model2) 

# coefficient for storage16/32/64 GB

summary(model2)$coef[2] 

# what % is the chance of selling a 16/32/64 GB better than 128GB

100*(exp(summary(model2)$coef[2])-1)

# probability of selling a 128GB iPad

predict(model2,newdata=data.frame(storage='128 GB'),type='response')

# Model 3 (all variables)

model3 <- glm(sold~startprice+biddable+condition+cellular+carrier+
                  color+storage+productline+noDescription+
                  upperCaseDescription+startprice_99end,
                  data=train, family='binomial')
summary(model3) 

# assess model performance on training data (.5 cutoff for predictions)

pred <- predict(model3, type='response')

ggplot(data=data.frame(pred),aes(x=pred))+
  geom_histogram(fill='steelblue3')

# how many predictions for sold?

table(as.numeric(pred>0.5))

# classification table

ct <- table(train$sold, pred > 0.5); ct

# accuracy

accuracy <- sum(diag(ct))/sum(ct); accuracy

# specificity

specific <- ct[1,1] / sum(ct[1,1], ct[1,2]); specific

# sensitivity

sensitive <- ct[2,2] / sum(ct[2,1], ct[2,2]); sensitive

# switch to test sample
# baseline prediction for test sample

baseline <- table(test$sold)[1]/nrow(test); baseline

# visualize

pred2 <- predict(model3, newdata=test, type='response')

ggplot(data=data.frame(pred2),aes(x=pred2))+
  geom_histogram(fill='steelblue3')

# classification table for test data

ct2 <- table(test$sold, pred2 > 0.5); ct2

# accuracy on test data

accuracy2 <- sum(diag(ct2))/sum(ct2); accuracy2

# specificity

specific2 <- ct2[1,1] / sum(ct2[1,1], ct2[1,2]); specific2

# sensitivity

sensitive2 <- ct2[2,2] / sum(ct2[2,1], ct2[2,2]); sensitive2

# test out accuracy of model for diff cutoffs

j = 1; acc = integer()
for (i in seq(0.10, 0.90, 0.01)){
  c = table(test$sold, pred2 > i)
  acc[j] = sum(c[1,1], c[2,2]) / nrow(test)
  j=j+1
}
ggplot(data=data.frame(cutoff=seq(0.1,0.9,0.01),accuracy=acc),aes(x=cutoff,y=accuracy))+
  geom_point()

# accuracy appears to peak around a cutoff of .6

# ROC curves

library(ROCR)

ROCRpred <- prediction(pred2, test$sold)
as.numeric(performance(ROCRpred, "auc")@y.values) # auc measure

# construct plot
ROCRperf <- performance(ROCRpred,"tpr","fpr")

plot(ROCRperf) # basic plot

plot(ROCRperf, colorize=TRUE) # color coded ROC curve

plot(ROCRperf, colorize=TRUE, print.cutoffs.at=seq(0,1,0.2), text.adj=c(-0.3,2)) 
# color coded and annotated ROC curve

#___________________________________________________________________________

# Feature Selection HTML File
# Load Wine data

wine <- read.table("winequality-white.csv", header=TRUE, sep=";")
str(wine)

set.seed(100)
wine_split = createDataPartition(y=wine$quality, p = 0.7, list = F, groups = 100)
wine_train = wine[wine_split,]
wine_test = wine[-wine_split,]

cor(wine_train[,-12])

# Large numbers indicate high correlations. 
# A good predictor has a high correlation with the outcome, 
# but low correlations with other predictors.

# simplify results

round(cor(wine_train[,-12]), 2)*100

# visualize correlation results

corMatrix <- as.data.frame(cor(wine_train[,-12]))
corMatrix$var1 = rownames(corMatrix)
corMatrix %>%
  gather(key=var2,value=r,1:11)%>%
  ggplot(aes(x=var1,y=var2,fill=r))+
  geom_tile()+
  geom_text(aes(label=round(r,2)),size=3)+
  scale_fill_gradient2(low = 'red',high='green',mid = 'white')+
  theme(axis.text.x=element_text(angle=90))

# there are also specific packages to build visuals

library(corrplot)
corrplot(cor(wine_train[,-12]),method = 'square',type = 'lower',diag = F)

# wine model

wine_model <- lm(quality ~ ., wine_train)
summary(wine_model)

# Threat of collinearity can also come from linear relationships between 
# sets of variables. 

# One way to assess the threat of multicollinearity in a linear regression 
# is to compute the Variance Inflating Factor (VIF). 1<VIF<Inf. 
# VIF>10 indicates serious multicollinearity. VIF>5 may warrant examination.

library(car)
vif(wine_model)

# Regsubsets compares different models with adjusted R2 or Mallow's Cp or BIC
# Process: Find the prediction error for each subset and use the subset 
# with the lowest prediction error

# This process can be slow and computationally intensive 
# because of a lot of cross-validation

# Since we have 11 predictors, regsubsets() will generate 
# 11 models to reflect the best combination for every set 
# (e.g., 1 predictor, two predictors,.. )

library(leaps)
subsets <- regsubsets(quality ~ ., data = wine_train, nvmax=11)
summary(subsets)
names(summary(subsets))

subsets_measures = data.frame(model=1:length(summary(subsets)$cp),
                              cp=summary(subsets)$cp,
                              bic=summary(subsets)$bic, 
                              adjr2=summary(subsets)$adjr2)

subsets_measures

subsets_measures %>%
  gather(key = type, value=value, 2:4)%>%
  ggplot(aes(x=model,y=value))+
  geom_line()+
  geom_point()+
  facet_grid(type~.,scales='free_y')

# Reg subset with lowest CP

which.min(summary(subsets)$cp) # 8

coef(subsets,which.min(summary(subsets)$cp)) # coefficients for this model

# Stepwise Variable Selection
# Forward Stepwise

start_mod <- lm(quality ~ 1, data = wine_train)
empty_mod <- lm(quality ~ 1, data = wine_train)
full_mod <- lm(quality ~ ., data = wine_train)

forwardStepwise <-step(start_mod,
                       scope=list(upper=full_mod,lower=empty_mod),
                       direction = 'forward')

summary(forwardStepwise)

# Backward Stepwise

start_mod2 = lm(quality ~ ., data = wine_train)
empty_mod2 = lm(quality ~ 1, data = wine_train)
full_mod2 = lm(quality ~ ., data = wine_train)

backwardStepwise <- step(start_mod2,
                         scope=list(upper=full_mod2, lower=empty_mod2),
                         direction = 'backward')

summary(backwardStepwise)

# Hybrid Stepwise

start_mod3 = lm(quality ~ 1, data = wine_train)
empty_mod3 = lm(quality ~ 1, data = wine_train)
full_mod3 = lm(quality ~ ., data = wine_train)

hybridStepwise <-step(start_mod3,
                      scope=list(upper=full_mod3,lower=empty_mod3),
                      direction='both')

summary(hybridStepwise)

# Ridge Regression

library(glmnet)

x = model.matrix(quality ~ . - 1, data = wine_train)
y = wine_train$quality

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

coef(cv.lasso)

# Dimension Reduction

trainPredictors = wine_train[,-12]
testPredictors = wine_test[,-12]

# Conduct Principal Components Analysis on train sample. 
# Principal components analysis will always generate as many components as variables.
# The first few components contain the most amount of variance. 
# One heuristic for number of components to retain is a 
# cumulative variance greater than 70%. 
# In this case, we are extracting only six of eleven components.

pca = prcomp(trainPredictors, scale. = T)
train_components = data.frame(cbind(pca$x[,1:6], quality = wine_train$quality))

head(train_components)

train_model = lm(quality ~ ., train_components)
summary(train_model)

# Before we can apply the train model to the test set, we need to ensure that we 
# apply the same variable transformations for the train sample on the test sample.

test_pca = predict(pca, newdata = testPredictors)
test_components = data.frame(cbind(test_pca[,1:6], quality = wine_test$quality))

str(train_components)
str(test_components)

pred = predict(train_model,newdata=test_components)
sse = sum((pred-test_components$quality)^2)
sst = sum((mean(train_components$quality) - test_components$quality)^2)
r2_test = 1 - sse/sst
r2_test

