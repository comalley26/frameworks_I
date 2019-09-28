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

# Assignment 7 - Trees

wages <- read.csv("assignment7_wages.csv")

head(wages)

str(wages) # sex and race are categorical

wages = wages[wages$earn>0,]  # remove rows with negative earning

mean(wages$sex == "female") # 0.627924

wages %>% 
  group_by(race) %>% 
  summarise(avg = mean(earn)) %>% 
  arrange(avg) # Hispanics earn the least on average

set.seed(100)
split = sample(1:nrow(wages), nrow(wages)*0.75) # 75% of data in training sample
train = wages[split,]
test = wages[-split,]

dim(train) # 1026 rows

model1 <- lm(earn ~ ., data = train)

summary(model1) # height, sex, ed, and age are significant based on p-values

pred1 <- predict(model1)

RMSE(pred = pred1, obs = train$earn)

sqrt(mean((train$earn - pred1)^2))/2 # 26567.31 (answer is half of RMSE - why?)

ggplot(data=train,aes(y=earn,x=sex,fill=factor(ed)))+ 
  geom_bar(stat="summary",fun.y="mean",position="dodge")

ggplot(data=train,aes(y=earn,x=ed,color=sex))+  
  geom_smooth(method="lm",se=F,size=1.2)+  
  scale_x_continuous(breaks=c(seq(2,20,2)))+  
  scale_y_continuous(breaks=c(seq(0,100000,20000)))

model_sex_ed = lm(earn~sex + ed + sex*ed,data=train)

summary(model_sex_ed) # ed and sexmale:ed are significant

model2 <- lm(earn ~ . + sex*ed, data = train)
pred2 <- predict(model2)
RMSE(pred = pred2, obs = train$earn) # 26516.04

model3 <- lm(earn ~ . + sex*ed + sex*age, data = train)
pred3 <- predict(model3)
RMSE(pred = pred3, obs = train$earn) # 26512.54

model4 <- lm(earn ~ . + sex*ed + sex*age + age*ed, data = train)
pred4 <- predict(model4)
RMSE(pred = pred4, obs = train$earn) # 26508.2

model5 <- lm(earn~(height+sex+race+ed+age)^2,data=train)
pred5 <- predict(model5)
RMSE(pred = pred5, obs = train$earn) # 26508.2

summary(model5)

tree1 <- rpart(earn ~ ., data = train) # 12 leaves on tree
prp(tree1, digits = 5) # tall people earn more, young people earn less

pred_tree1 <- predict(tree1)

RMSE(pred = pred_tree1, obs = train$earn) # 24367.89

treeSimp1 <- rpart(earn ~ ., data = train,
                   control=rpart.control(minbucket=20))

prp(treeSimp1, digits = 5) # 9 leaves

pred_treeSimp1 <- predict(treeSimp1)

RMSE(pred = pred_treeSimp1, obs = train$earn) # 25466.95

treeSimp2 <- rpart(earn ~ ., data = train,
                   control=rpart.control(minbucket=50))

prp(treeSimp2, digits = 5) # 7 leaves

pred_treeSimp2 <- predict(treeSimp2) 

RMSE(pred = pred_treeSimp2, obs = train$earn) # 26328.55

treeComplex1 <- rpart(earn ~ ., data = train,
                      control=rpart.control(minbucket=5))

prp(treeComplex1, digits = 5) # 12 leaves

pred_treeComplex1 <- predict(treeComplex1) 

RMSE(pred = pred_treeComplex1, obs = train$earn) # 24348.58 

treeComplex2 <- rpart(earn ~ ., data = train,
                      control=rpart.control(minbucket=1))

prp(treeComplex2, digits = 5) # 15 leaves

pred_treeComplex2 <- predict(treeComplex2) 

RMSE(pred = pred_treeComplex2, obs = train$earn) # 23180.9 (lowest RMSE)

# Last part

test_pred5 <- predict(model5, newdata = test)
RMSE(pred = test_pred5, obs = test$earn) # 27949.29 (best performance for this part)

test_tree1 <- predict(tree1, newdata = test)
RMSE(pred = test_tree1, obs = test$earn) # 29545.45

test_treeSimp2 <- predict(treeSimp2, newdata = test)
RMSE(pred = test_treeSimp2, obs = test$earn) # 28238.25

pred_treeComplex2_test <- predict(treeComplex2, newdata = test)
RMSE(pred = pred_treeComplex2_test, obs = test$earn) # 28888.88

# Assignment 8

str(OJ)
set.seed(1234)

oj_index <- sample.split(Y = OJ$Purchase, SplitRatio = .7)

train <- OJ[oj_index, ]
test <- OJ[-oj_index, ]

dim(train) # 749 rows

dim(train[train$Purchase == "MM", ]) # 292 MM purchases

mean(train$PriceMM) # 2.087223

mean(train$DiscMM) # 0.1237116

train[train$WeekofPurchase == 275 & train$Purchase == "MM", ] %>% nrow() 
# 17 MM purchases

tree1 <- rpart(Purchase ~ PriceCH+PriceMM+DiscCH+DiscMM+SpecialCH+SpecialMM+
                          LoyalCH+PriceDiff+PctDiscMM+PctDiscCH,
               data = train,
               method = "class")

pred = predict(tree1, newdata = test, type = "prob")

ROCRpred = prediction(pred[,2], test$Purchase)
summary(ROCRpred)
as.numeric(performance(ROCRpred,"auc")@y.values) # auc measure

# 10 fold cross validation 

trControl = trainControl(method="cv", number = 10) # 10-fold cross validation

tuneGrid = expand.grid(.cp=seq(0,0.1,0.001))  

set.seed(100)

trainCV = train(Purchase ~ PriceCH+PriceMM+DiscCH+DiscMM+SpecialCH+SpecialMM+
                           LoyalCH+PriceDiff+PctDiscMM+PctDiscCH,
                data=train,
                method="rpart", 
                trControl=trControl,
                tuneGrid=tuneGrid)

head(trainCV$results) # first few cv results

plot(trainCV)

trainCV$bestTune # best complexity parameter (CP) is 0.004

tree1 <- rpart(Purchase ~ PriceCH+PriceMM+DiscCH+DiscMM+SpecialCH+SpecialMM+
                 LoyalCH+PriceDiff+PctDiscMM+PctDiscCH,
               data = train,
               method = "class", 
               cp = 0.004)

pred = predict(tree1, newdata = test)

ROCRpred = prediction(pred[,2], test$Purchase)

as.numeric(performance(ROCRpred,"auc")@y.values) # same auc measure as before

summary(tree1)

# Bag
# Bagging using Random Forest (specified by mtry argument)

set.seed(100)

bag <- randomForest(Purchase ~ PriceCH+PriceMM+DiscCH+DiscMM+SpecialCH+SpecialMM+
                               LoyalCH+PriceDiff+PctDiscMM+PctDiscCH,
                    data=train,
                    mtry = 10,
                    ntree = 1000)

plot(bag)
varImpPlot(bag)

predBag <- predict(bag, newdata = test, type = "prob")

ct <- table(test$Purchase, predBag[,2]>.5)
sum(diag(ct))/sum(ct)

ROCRpred = prediction(predBag[,2]>.5, test$Purchase)

as.numeric(performance(ROCRpred,"auc")@y.values) # same auc measure as before

bag = randomForest(Purchase~PriceCH+PriceMM+DiscCH+DiscMM+SpecialCH+
                            SpecialMM+LoyalCH+PriceDiff+PctDiscMM+PctDiscCH,
                   data=train, 
                   mtry=10,
                   ntree=1000)

predBag = predict(bag,newdata = test, type = 'prob')
ROCRpredBag = prediction(predBag[,2],test$Purchase)
as.numeric(performance(ROCRpredBag,"auc")@y.values) # auc measure









