getwd()

library(readxl)
library(dplyr)
library(ggplot2)
library(tidyr)
library(readr)
library(caTools)
library(broom)

ebay <- read.csv("Frameworks I/EbayAssignment.csv")

str(ebay)
glimpse(ebay)

# Part I
# find number of rows in dataset

nrow(ebay)
ncol(ebay)

# find number of black iPads in dataset

ebay %>%
  filter(color == "Black") %>%
  summarise(n())

# which types of iPads are sold on eBay?

as.vector(unique(ebay$productline))

# find uniqueID of iPad with highest startprice

ebay %>%
  filter(startprice == max(startprice)) %>%
  select(UniqueID)

# Part II
# split samples using caTools 

set.seed(196)

train_index <- sample.split(Y = ebay$sold,
                            SplitRatio = 4/5)

etrain <- ebay[train_index, ]

etest <- ebay[!train_index, ]

# number of rows in training dataset

nrow(etrain)

# median startprice of iPads that sold

etrain %>%
  filter(sold == 1) %>%
  summarise(median(startprice))

# median startprice of iPads that did not sell

etrain %>%
  filter(sold == 0) %>%
  summarise(median(startprice))

# log regression to predict sold

model1 <- glm(sold ~ biddable + 
           startprice + 
           condition + 
           cellular + 
           carrier + 
           color + 
           storage +
           productline +
           noDescription + 
           charCountDescription + 
           upperCaseDescription +
           startprice_99end, 
    data = etrain,
    family = "binomial")

summary(model1)

model1$aic

# create tibble with glm results

tidy_model1 <- tidy(model1)

model1$coefficients

View(tidy_model1)

unique(tidy_model1$term)

tidy_model1 %>%
  filter(p.value < 0.10) %>%
  arrange(p.value)

unique(etrain$productline)

# Part III
# New linear model

ltrain2 <- glm(sold ~ biddable+startprice+condition+storage+productline+
                      upperCaseDescription+startprice_99end,
               data = etrain,
               family = "binomial")

summary(ltrain2)

tidy_ltrain2 <- tidy(ltrain2)

tidy_ltrain2 %>% arrange(p.value)

names(ltrain2$coefficients)

# percent change in likelihood of selling per $1 increase in startprice

exp(ltrain2$coefficients['startprice'])

# how much more likely to sell iPad Air 1/2 vs iPad 1 (dummy variable)

exp(ltrain2$coefficients['productlineiPad Air 1/2'])

# new model with only productline

model_production <- glm(sold~productline, data= train, family = 'binomial')
model_production$coefficients

# Part IV
# predict using model2

pred <- predict(ltrain2, newdata = etest, type = "response")
summary(pred)

pred[etest$UniqueID==10940]

pred

# determine accuracy of predictions

ct <- table(test$sold, pred>0.5)
acc1 <- sum(diag(ct))/sum(ct)
acc1

# how much better is our model than a random guess that it won't be sold?

ct2 <- table(test$sold, pred==0); ct2
acc2 <- sum(diag(ct2))/sum(ct2)
acc2

acc1 - acc2

(acc1 * 100) / (acc2 * 100)

# find AUC of model

install.packages('ROCR')    
library(ROCR)
ROCRpred = prediction(pred,test$sold)
as.numeric(performance(ROCRpred,"auc")@y.values) # auc measure

## construct plot
ROCRperf = performance(ROCRpred,"tpr","fpr")
plot(ROCRperf,colorize=TRUE,print.cutoffs.at=seq(0,1,0.2),text.adj=c(-0.3,2),xlab="1 - Specificity",ylab="Sensitivity") # color coded, annotated ROC curve





