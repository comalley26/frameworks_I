getwd()
setwd("C:/Users/casey/OneDrive/Documents/Frameworks I")

houses <- read.csv("houses.csv")

library(caret)
library(dplyr)
library(tidyr)
library(ggplot2)
library(broom)

names(houses)
str(houses)

# set up train and test data

set.seed(1031)

train_index <- createDataPartition(houses$price, p = 0.7, list = F, groups = 100)

train <- houses[train_index, ]            
test <- houses[-train_index, ]

# avg price of training houses

mean(train$price)

# model training price

model1 <- lm(price ~ . - id, data = train)

tidy(model1) %>% arrange(p.value) #sqft_lot lowest out of choices

# which has lowest r-squared?

lm(price ~ bedrooms, data = train) %>% summary()
lm(price ~ bathrooms, data = train) %>% summary()
lm(price ~ sqft_lot, data = train) %>% summary()
lm(price ~ condition, data = train) %>% summary()
lm(price ~ grade, data = train) %>% summary()

# code from professor in quiz

#install.packages("corrplot")
library(corrplot)
corrplot(cor(train[,c(3:7, 10:13,16)]),method = 'square',type = 'lower',diag = F)

# correlation between sqft living and sqft above & basement

cor(train$sqft_living, train$sqft_above + train$sqft_basement)

vif_model <- lm(price ~ bedrooms + bathrooms + sqft_living + sqft_lot + floors + waterfront +
                        view + condition + grade + age,
                data = train)

# find variable with highest VIF

#install.packages("car")
library(car)
vif(vif_model) %>% tidy() %>% arrange(desc(x))

# Part II
# find best subset of 6 variables in model2

train_sub <- train[ , c("price", "bedrooms", "bathrooms", "sqft_living", "sqft_lot", 
                        "floors", "waterfront", "view", "condition", "grade", "age")]

test_sub <- test[ , c("price", "bedrooms", "bathrooms", "sqft_living", "sqft_lot", 
                        "floors", "waterfront", "view", "condition", "grade", "age")]

model2 <- lm(price ~ bedrooms + bathrooms + sqft_living + 
                     sqft_lot + floors + waterfront + view + 
                     condition + grade + age,
             data = train)

summary(model2)

model2 <- lm(price ~ ., data = train_sub)

#install.packages("olsrr")
#install.packages('leaps')
#install.packages("stats")
library(leaps)
library(stats)
library(olsrr)

# subset selection model

model2sub <- ols_step_best_subset(model2)

# rsquare with 6 variables

model2sub %>% filter(n == 6) %>% select(rsquare)

# forward stepwise model using AIC

ols_step_forward_aic(model2)

# backward stepwise model using AIC

ols_step_backward_aic(model2)

# hybrid stepwise model using AIC

ols_step_both_aic(model2)

# lasso regression

install.packages("glmnet")
library(glmnet)
?cv.glmnet

tsub_matrix <- data.matrix(train_sub)

cv.glmnet(tsub_matrix, price, lambda = 10, alpha = 1)

xfactors <- model.matrix(price ~ bedrooms + bathrooms + sqft_living + 
                           sqft_lot + floors + waterfront + view + 
                           condition + grade + age, data = train)

# from youtube video

library(Matrix)
train_sub
test_sub

train_sparse <- sparse.model.matrix(~., train_sub[2:11])
test_sparse <- sparse.model.matrix(~., test_sub[2:11])

cv_fit <- cv.glmnet(train_sparse, train_sub[, 1], nfolds = 10, alpha = 1, family = "gaussian")

# coefficients used in best model

coef(cv_fit)

#find R-squared 

cf <- coef(cv_fit, s = "lambda.1se"); cf
i <- which(cv_fit$lambda == cv_fit$lambda.1se); i
e <- cv_fit$cvm[i]
r2 <- 1-e/var(test_sub$price); r2

cv_pred <- predict(cv_fit, test_sparse, s = cv_fit$lambda.min)
cv_pred

cv_eval <- data.frame(price = test$price, pred = cv_pred)
cv_eval

cv_eval <- cv_eval %>% mutate(diff = price - pred)

cv_sse <- sum(cv_eval$diff^2); cv_sse
cv_sst <- sum((mean(train_sub$price) - cv_eval$price)^2); sst

cv_test_r2 <- 1 - cv_sse/cv_sst
cv_test_r2


# Note alpha=1 for lasso only and can blend with ridge penalty down to
# alpha=0 ridge only.
lasso_model2 <- cv.glmnet(x = xfactors, y = train$price, alpha = 1, nlambda = 10, family = "gaussian")
class(train)
train

# Plot variable coefficients vs. shrinkage parameter lambda.
summary(lasso_model2)

# Dimension reduction: 
# Now, rather than selecting individual variables, we will capture the essence in a few components 
# so as to retain at least 90% of the information. 

trainPredictors = train[,c(3:11,16)]
testPredictors = test[,c(3:11,16)]
x = preProcess(x = trainPredictors,method = 'pca',thresh = 0.9)
trainComponents = predict(x,newdata=trainPredictors)
trainComponents$price = train$price

trainComponents
length(names(trainComponents))
names(trainComponents)

tc_model <- lm(price ~ ., data = trainComponents)
summary(tc_model)

# last question

testComponents = predict(x,newdata=testPredictors)
testComponents$price = test$price

testComponents

pred <- predict(tc_model, newdata = testComponents)

summary(pred)

sse <- sum((testComponents$price - pred)^2); sse
sst <- sum((mean(train$price) - testComponents$price)^2); sst

test_r2 <- 1 - sse/sst
test_r2












