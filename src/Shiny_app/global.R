#Shiny app for the Amylase data
#by Chris Morano

#global.R

#load the libraries:
library(tidyverse)
library(shiny)
library(shinythemes)
library(randomForest)

########################

#load the data:
data <- read_csv('data/Aug21_summarized_1week.csv')

#data for 0 vs ABC:
#filter out patients with only one test result
model_0vsABC_data <- data %>% 
  filter(num_of_results > 1)

#Model 1: 0 vs ABC:
#combine A, B, and C:
model_0vsABC_data$Result_2016[model_0vsABC_data$Result_2016 %in% c('A', 'B', 'C')] <- 'ABC'
model_0vsABC_data$Result_2016 <- factor(model_0vsABC_data$Result_2016)

#training and test set:
set.seed(132)
model_0vsABC_data <- sample_frac(model_0vsABC_data)

model_0vsABC_train <- model_0vsABC_data[1:832,]
model_0vsABC_test  <- model_0vsABC_data[833:1040,]

#features to use: min_result, mean_result, max_result, var_result, diff_of_last_and_max & diff_of_last_and_min:
model_0vsABC_train_X <- model_0vsABC_train %>% 
  select(min_result:var_result, diff_of_last_and_max, diff_of_last_and_min)
model_0vsABC_train_y <- model_0vsABC_train$Result_2016

model_0vsABC_test_X <- model_0vsABC_test %>% 
  select(min_result:var_result, diff_of_last_and_max, diff_of_last_and_min)
model_0vsABD_test_y <- model_0vsABC_test$Result_2016


########################
#random_forest_model:

set.seed(137)
model_0vsABC.rf <- randomForest(x=model_0vsABC_train_X, y=model_0vsABC_train_y, type='prob', 
                                maxnodes=150, importance=TRUE, proximity = TRUE, keep.forest=TRUE)

########################



