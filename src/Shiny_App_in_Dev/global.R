#Shiny app for the Amylase data
#by Chris Morano

#global.R

#load the libraries:
library(tidyverse)
library(shiny)
library(shinydashboard)
library(shinyBS)
library(caret)
library(randomForest)
library(lime)
library(e1071)
library(pROC)


#MODEL 1: 0 vs ABC:
#----------------------------------------------------

#importing data for 0 vs ABC:
data.0vsABC <- read_csv('data/med_data.0vsABC.csv')
data.0vsABC[data.0vsABC$Result_2016 == '0',]$Result_2016 <- 'O'
data.0vsABC$Result_2016 <- factor(data.0vsABC$Result_2016, levels=c('O', 'ABC'))

#shuffling the data:
set.seed(1315)
data.0vsABC.shuf <- sample_frac(data.0vsABC)  #1040 rows

#training and test sets:
n1 <- round(nrow(data.0vsABC.shuf))
data.0vsABC.train <- data.0vsABC.shuf[1:(n1*.75),]
data.0vsABC.test <- data.0vsABC.shuf[((n1*.75)+1):n1,]

#running the random forest model:
# data.0vsABC.rf <- randomForest(
#   Result_2016 ~ max_result + diff_of_last_and_max + diff_of_last_and_min, 
#   data=data.0vsABC.train, 
#   xtest=select(data.0vsABC.test, max_result, diff_of_last_and_max, diff_of_last_and_min), 
#   ytest=data.0vsABC.test$Result_2016,
#   maxnodes = 100, type='prob', importance=TRUE, proximity = TRUE, keep.forest=TRUE)


#the newer version:
data.0vsABC.rf <- caret::train(Result_2016 ~ max_result + diff_of_last_and_max + diff_of_last_and_min,
                               data=data.0vsABC.train, method = "rf", 
#                               xtest=select(data.0vsABC.test, max_result, diff_of_last_and_max, diff_of_last_and_min),
#                               ytest=data.0vsABC.test$Result_2016,
                               trControl = trainControl(classProbs = TRUE))

#lime explanation for 0 vs ABC:
explainer.0vsABC <- lime(data.0vsABC.train, data.0vsABC.rf, bin_continuous = FALSE, n_permutations = 100)

explanation.0vsABC <- lime::explain(data.0vsABC.train[2,], explainer.0vsABC, n_labels=2, n_features=3)

#getting the metrics:
data.0vsABC.mroc <- roc(ifelse(data.0vsABC.test$Result_2016 == "O", 0, 1),
                        (as.numeric(predict(data.0vsABC.rf, data.0vsABC.test))-1), plot=TRUE)

data.0vsABC.auc <- auc(ifelse(data.0vsABC.test$Result_2016 == "O", 0, 1), 
                       (as.numeric(predict(data.0vsABC.rf, data.0vsABC.test))-1))

data.0vsABC.coords <- coords(data.0vsABC.mroc, .5, "threshold", 
                             ret=c("accuracy","sensitivity","specificity","ppv","npv"))

#MODEL 2: 0A vs BC:
#----------------------------------------------------

#model 2 data for 0A vs BC:
data.0AvsBC <- read.csv('data/med_data.0AvsBC.csv')
data.0AvsBC$Result_2016 <- factor(data.0AvsBC$Result_2016)

#shuffling the data:
set.seed(131)
data.0AvsBC.shuf <- sample_frac(data.0AvsBC)

#training and test sets:
n2 <- round(nrow(data.0AvsBC.shuf))
data.0AvsBC.train <- data.0AvsBC.shuf[1:(n2*.75),]
data.0AvsBC.test <- data.0AvsBC.shuf[((n2*.75)+1):n2,]

#running a random forest model:
# data.ABC.rf <- randomForest(
#   Result_2016 ~ max_result + diff_of_last_and_max + diff_of_last_and_min,
#   data=data.ABC.train,
#   xtest=select(data.ABC.test, max_result, diff_of_last_and_max, diff_of_last_and_min),
#   ytest=data.ABC.test$Result_2016,
#   maxnodes = 100, type='prob', importance=TRUE, proximity = TRUE, keep.forest=TRUE,
#   cutoff=c(0.38, 0.38, 0.24), mtry = 3)

#the newer model:
data.0AvsBC.rf <- caret::train(Result_2016 ~ max_result + diff_of_last_and_max + diff_of_last_and_min,
                               data=data.0AvsBC.train, method = "rf", 
#                               xtest=select(data.0AvsBC.test, max_result, diff_of_last_and_max, diff_of_last_and_min),
#                               ytest=data.0AvsBC.test$Result_2016,
                               trControl = trainControl(classProbs = TRUE))

#the lime explanation:
explainer.0AvsBC <- lime(data.0AvsBC.train, data.0AvsBC.rf, bin_continuous = FALSE, n_permutations = 100)

explanation.0AvsBC <- lime::explain(data.0AvsBC.train[2,], explainer.0AvsBC, n_labels=2, n_features=3)

#getting the metrics:
data.0AvsBC.mroc <- roc(ifelse(data.0AvsBC.test$Result_2016 == "OA", 0, 1),
                        (abs(as.numeric(predict(data.0AvsBC.rf, data.0AvsBC.test))-2)), plot=TRUE)

data.0AvsBC.auc <- auc(ifelse(data.0AvsBC.test$Result_2016 == "OA", 0, 1), 
                       (abs(as.numeric(predict(data.0AvsBC.rf, data.0AvsBC.test))-2)))

data.0AvsBC.coords <- coords(data.0AvsBC.mroc, .5, "threshold", 
                             ret=c("accuracy","sensitivity","specificity","ppv","npv"))

#MODEL 3: A vs B vs C:
#----------------------------------------------------

#model 3 data for A vs B vs C:
data.ABC <- read.csv('data/med_data.AvsBvsC.csv')
data.ABC$Result_2016 <- factor(data.ABC$Result_2016)

#shuffling the data:
set.seed(131)
data.ABC.shuf <- sample_frac(data.ABC)

#training and test sets:
n3 <- round(nrow(data.ABC.shuf))
data.ABC.train <- data.ABC.shuf[1:(n3*.75),]
data.ABC.test <- data.ABC.shuf[((n3*.75)+1):n3,]

#running a random forest model:
# data.ABC.rf <- randomForest(
#   Result_2016 ~ max_result + diff_of_last_and_max + diff_of_last_and_min,
#   data=data.ABC.train,
#   xtest=select(data.ABC.test, max_result, diff_of_last_and_max, diff_of_last_and_min),
#   ytest=data.ABC.test$Result_2016,
#   maxnodes = 100, type='prob', importance=TRUE, proximity = TRUE, keep.forest=TRUE,
#   cutoff=c(0.38, 0.38, 0.24), mtry = 3)

#the newer model:
data.ABC.rf <- caret::train(Result_2016 ~ max_result + diff_of_last_and_max + diff_of_last_and_min,
                            data=data.ABC.train, method = "rf",
#                            xtest=select(data.ABC.test, max_result, diff_of_last_and_max, diff_of_last_and_min),
#                            ytest=data.ABC.test$Result_2016,
                            trControl = trainControl(classProbs = TRUE))

#the lime explanation:
explainer.ABC <- lime(data.ABC.train, data.ABC.rf, bin_continuous = FALSE, n_permutations = 100)

explanation.ABC <- lime::explain(data.ABC.train[3,], explainer.ABC, n_labels=3, n_features=3)


#getting the metrics:
data.ABC.mroc <- roc(ifelse(data.ABC.test$Result_2016 == "C", 1, 0),
                     ifelse(predict(data.ABC.rf, data.ABC.test) == "C", 1, 0), plot=TRUE)

data.ABC.auc <- auc(ifelse(data.ABC.test$Result_2016 == "C", 1, 0), 
                    ifelse(predict(data.ABC.rf, data.ABC.test) == "C", 1, 0))

data.ABC.coords <- coords(data.ABC.mroc, .5, "threshold", 
                             ret=c("accuracy","sensitivity","specificity","ppv","npv"))

