getwd()
setwd("C:/Users/casey/OneDrive/Documents/Frameworks I")

wages <- read.csv("assignment7_wages.csv")

library(dplyr)
library(rpart)
library(rpart.plot)
library(ggplot2)
library(broom)
library(tidyr)
library(caret)

# Part I
# Q1: Non-metrics are Sex and Race (discrete)

summary(wages)
names(wages)
dim(wages)


# Q2: remove negatives and find percent of female respondents

wages <- wages[wages$earn>0,]

wages %>%
  group_by(sex) %>%
  summarise(n = n()) # 859 females and 509 males

859 / (859 + 509) # fraction female = .627924


# Q3: which race earns the least?

wages %>%
  group_by(race) %>%
  summarise(avg = mean(earn),
            med = median(earn)) %>%
  arrange(avg) # Hispanics earn the least


# Q4: 75% of data in training set

set.seed(100)
split = sample(1:nrow(wages), nrow(wages)*0.75)
train = wages[split,]
test = wages[-split,]

# Q5: 1026 observations
dim(train) 


# Part II
# Q1: linear model 

model1 <- lm(earn ~ .,
             data = train)

summary(model1)

tidy1 <- tidy(model1)

tidy1 %>% filter(p.value > 0.05) #race is only non-significant variable

# Q2: RMSE of training sample

pred1_train <- predict(model1, newdata = train)
pred1_train

RMSE(pred = pred1_train, obs = train$earn) # RMSE = 26567.31

# Q3 and Q4: ggplots to identify increase in earnings for years of ed

ggplot(data=train,aes(y=earn,x=sex,fill=factor(ed))) + 
  geom_bar(stat="summary",fun.y="mean",position="dodge")

ggplot(data=train,aes(y=earn,x=ed,color=sex)) + 
  geom_smooth(method="lm",se=F,size=1.2) +
  scale_x_continuous(breaks=c(seq(2,20,2))) +  
  scale_y_continuous(breaks=c(seq(0,100000,20000))) # male 20000, female 15000 from 12 to 16 years

# Q5: new model with interaction variable

model_sex_ed <- lm(earn~sex + ed + sex*ed,
                   data=train)

tidy(model_sex_ed) %>% filter(p.value < 0.05) # ed and sexmale:ed are significant

# Q7: model2 is model1 with interaction variable

model2 <- lm(earn ~ . + sex*ed,
             data = train)

pred2_train <- predict(model2, newdata = train)
pred2_train

RMSE(pred = pred2_train, obs = train$earn) # RMSE = 26516.03

# Q9: model3

model3 <- lm(earn ~ . + sex*ed + sex*age,
             data = train)

pred3_train <- predict(model3, newdata = train)
pred3_train

RMSE(pred = pred3_train, obs = train$earn) # RMSE = 26512.54

# Q10: model4

model4 <- lm(earn ~ . + sex*ed + sex*age + age*ed,
             data = train)

pred4_train <- predict(model4, newdata = train)
pred4_train

RMSE(pred = pred4_train, obs = train$earn) # RMSE = 26508.2

# Q11: model5

model5 <- lm(earn~(height+sex+race+ed+age)^2,
             data=train)

pred5_train <- predict(model5, newdata = train)
pred5_train

RMSE(pred = pred5_train, obs = train$earn) # RMSE = 26261.21

# Q12: significant values for model5

tidy(model5) %>% filter(p.value < .05) # no significant p.values


# Part III
# tree model

tree1 <- rpart(earn ~ .,
               data = train)

prp(tree1,digits=5) # sex is first split variable

# Q4: 12 leaves

# Q5: RMSE for tree1 

tree1_train <- predict(tree1, newdata = train)
tree1_train

RMSE(pred = tree1_train, obs = train$earn) # RMSE = 24367.89

# Q6: treeSimp1

treeSimp1 <- rpart(earn ~ ., 
                   data = train,
                   control=rpart.control(minbucket=20))

prp(treeSimp1,digits=5) # 9 leaves

# Q7: RMSE for treeSimp1

treeSimp1_train <- predict(treeSimp1, newdata = train)
treeSimp1_train

RMSE(pred = treeSimp1_train, obs = train$earn) # RMSE = 25466.95

# Q8: treeSimp2

treeSimp2 <- rpart(earn ~ ., 
                   data = train,
                   control=rpart.control(minbucket=50))

prp(treeSimp2,digits=5) # 7 leaves

# Q9: RMSE for treeSimp2

treeSimp2_train <- predict(treeSimp2, newdata = train)
treeSimp2_train

RMSE(pred = treeSimp2_train, obs = train$earn) # RMSE = 26328.55

# Q10: treeComplex1

treeComplex1 <- rpart(earn ~ ., 
                   data = train,
                   control=rpart.control(minbucket=5))

treeComplex1_train <- predict(treeComplex1, newdata = train)
treeComplex1_train

RMSE(pred = treeComplex1_train, obs = train$earn) # RMSE = 24348.58

# Q11: treeComplex2

treeComplex2 <- rpart(earn ~ ., 
                      data = train,
                      control=rpart.control(minbucket=1))

treeComplex2_train <- predict(treeComplex2, newdata = train)
treeComplex2_train

RMSE(pred = treeComplex2_train, obs = train$earn) # RMSE = 23180.9

# Part IV

# Q1: test linear models on testing data

# model 1

pred1_test <- predict(model1, newdata = test)
pred1_test

RMSE(pred = pred1_test, obs = test$earn) # RMSE = 28439.56

# model 2

pred2_test <- predict(model2, newdata = test)
pred2_test

RMSE(pred = pred2_test, obs = test$earn) # RMSE = 28384.32

# model 3

pred3_test <- predict(model3, newdata = test)
pred3_test

RMSE(pred = pred3_test, obs = test$earn) # RMSE = 28315.4

# model 4

pred4_test <- predict(model4, newdata = test)
pred4_test

RMSE(pred = pred4_test, obs = test$earn) # RMSE = 28305.82

# model 5

pred5_test <- predict(model5, newdata = test)
pred5_test

RMSE(pred = pred5_test, obs = test$earn) # RMSE = 27949.29 (lowest RMSE for linear models)

# Q2: test RMSE for tree1

tree1_test <- predict(tree1, newdata = test)
tree1_test

RMSE(pred = tree1_test, obs = test$earn) # RMSE = 29545.45

# Q3: test RMSE for treeSimp2

treeSimp2_test <- predict(treeSimp2, newdata = test)
treeSimp2_test

RMSE(pred = treeSimp2_test, obs = test$earn) # RMSE = 28238.25

# Q4: test RMSE for treeComplex2

treeComplex2_test <- predict(treeComplex2, newdata = test)
treeComplex2_test

RMSE(pred = treeComplex2_test, obs = test$earn) # RMSE = 28888.88

# Q5: lowest test RMSE was for linear model5 
















