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

?sample.split

train_index <- sample.split(Y = ebay$sold,
                      SplitRatio = 4/5)

etrain <- ebay[train_index, ]

etest <- ebay[-train_index, ]

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

?glm

ltrain <- glm(sold ~ biddable + 
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

summary(ltrain)

ltrain$aic

summary(ltrain) %>%
  names()

# create tibble with glm results

tidy_ltrain <- tidy(ltrain)

tidy_ltrain

tidy_ltrain %>%
  filter(p.value < 0.10)

unique(etrain$productline)

ltrain$aic










