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

# Assignment 2

mean(diamonds$carat) # 0.7979397

mean(diamonds$carat[diamonds$cut == "Ideal"]) # 0.702837

diamonds %>%
  group_by(cut) %>%
  summarise(v = var(carat)) %>%
  arrange(v) # Fair has largest variance

diamonds %>%
  filter(color == "D") %>%
  group_by(cut) %>%
  summarise(n = n()) # Ideal has greatest selection of D color diamonds

diamonds %>%
  filter(carat > 1) %>%
  summarise(euros = mean(price)*.85) # 7140 (need to do other way to get decimals)

mean(diamonds$price[diamonds$carat > 1]) * .85 # 7140.268

ggplot(diamonds, aes(x = price, col = cut)) +
  geom_density() # Ideal cuts don't look more expensive (false)

ggplot(data=diamonds,aes(x=carat))+ 
  geom_histogram(binwidth = 0.01)+
  coord_cartesian(xlim=c(0,2.5))+
  scale_x_continuous(breaks=seq(0,2.5,0.1)) # .3| .4| .5| .7| .9| 1.0| 1.2| 1.5| 2.0

library(tidyr)
model = paste('model',1:10,sep = '')
sse = runif(10,min = 4000,max = 10000)
rmse = sqrt(sse)
r2 = ((rmse - min(rmse))/(max(rmse)-min(rmse)))*0.9
results = data.frame(model, sse, rmse, r2)
results

results %>% gather(key=metric, value=value, 2:4) # Solution for 9

library(ggplot2); data(diamonds); diamonds$x[diamonds$x==0] = NA
mean(diamonds$x, na.rm=T) # Solution for 10

# Assignment 3

# Support Vector Machines are least interpretable but make good predictions

# Understanding factors that affect a variable = inference approach
# only predictive if using the information to model and forecast data

# Choosing between two brands is a classification problem

# Model complexity goes up = prediction error goes down

# Model complexity goes up = test error goes down then up

set.seed(100)

train_index <- sample(1:nrow(mpg), .8 * nrow(mpg))

train <- mpg[train_index, ]
test <- mpg[-train_index, ]

summary(mpg); summary(train); summary(test)

mean(train$hwy) - mean(test$hwy) # 0.8969166 difference in avg hwy

train_index2 <- createDataPartition(mpg$hwy, p = 0.8, groups = 20, list = FALSE)

train2 <- mpg[train_index2, ]
test2 <- mpg[-train_index2, ]

summary(mpg); summary(train2); summary(test2)

mean(train2$hwy) - mean(test2$hwy) # -0.08732466 difference in avg hwy

oj_index <- sample.split(Y = OJ$Purchase, SplitRatio = 0.6)

ojtrain <- OJ[oj_index, ]
ojtest <- OJ[!oj_index, ]

dim(OJ); dim(ojtrain); dim(ojtest)

ojtrain[ojtrain$Purchase == "MM", ] %>% dim() # 250 MM purchases in training data

# P-Values must be low to reject null hypothesis

# Level of Significance should be set before analysis

# Assignment 4

houses <- read.csv("houses.csv")

set.seed(1031)

train_index <- createDataPartition(y = houses$price, p = .7, groups = 100, list = F)

train <- houses[train_index, ]
test <- houses[-train_index, ]

mean(train$price) # 540674.2

mean(test$price) # 538707.6

train %>%
  select(id,price:sqft_lot,age)%>%
  gather(key=numericVariable,value=value,price:age)%>%
  ggplot(aes(x='',y=value))+
  geom_boxplot(outlier.color = 'red')+
  facet_wrap(~numericVariable,scales='free_y')

train$sqft_living[train$bedrooms == max(train$bedrooms)] # 1620

plot(train$sqft_living, train$price) # direct relationship

cor(train$sqft_living, train$price) # 0.7060823

model1 <- lm(price ~ sqft_living, data = train)

summary(model1) # p-value is statistically significant (less than .05)

# R-squared = 0.4986

pred1 <- predict(model1)

RMSE(obs = train$price, pred = pred1) # 263932.6

predict(model1, newdata = data.frame(sqft_living = 1400)) # predicted cost = 346581

summary(model1)$coef[2] * 200 # price increase for 200ft extension is 56980.49

model2 <- lm(price ~ waterfront, data = train)

summary(model2) # r-squared is 0.07407; p-value is statistically significant

# Coefficient is 1179766 (expected increase in house price for waterfront view)

pred2 <- predict(model2)

RMSE(obs = train$price, pred = pred2) # 358649.4 (higher than model 1)

model3 <- lm(price ~ waterfront + sqft_living, data = train)

summary(model3) # r-squared is 0.0.5375 (higher than models 1 and 2)
# p-value is statistically significant

# Waterfront coefficient is 861002.363

model4 <- lm(price ~ bedrooms + bathrooms + sqft_living + sqft_lot + 
             floors + waterfront + view + condition + grade + age,
             data = train)

summary(model4) # r-squared is 0.6513

pred4 <- predict(model4)

RMSE(pred = pred4, obs = train$price) # 220098.4

sqrt(mean((pred4 - train$price)^2)) # Manual RMSE calc: 220098.4

# Model 4 has lowest RMSE so far

summary(model4)$coef[3] # bathrooms coefficient is 50744.76

tidy(model4) %>% arrange(p.value) # sqft_living has lowest p-value

test_pred4 <- predict(model4, newdata = test)

rss <- sum((test_pred4 - test$price)^2)
tss <- sum((mean(test$price) - test$price)^2)

1 - rss/tss # r-squared is 0.6544801

RMSE(pred = test_pred4, obs = test$price) # 207835.2

