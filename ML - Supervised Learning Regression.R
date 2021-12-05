# load library
library(tidyverse)
library(caret)
library(e1071)
library(mlbench)
library(rpart) #decision tree
library(ranger) #random forest

#load data set
data("BostonHousing")
boston <- BostonHousing

#prep data > train > score > evaluate
#1.split data
set.seed(27)
id <- createDataPartition(y = boston$medv, 
                          p = 0.8,
                          list = FALSE)

train_data <- boston[id, ] #train data 80%
test_data <- boston[-id, ] #test data 20%

#2.train model
set.seed(27)

ctrl <- trainControl(method = "repeatedcv",
                     number = 5,
                     repeats = 5,
                     verboseIter = TRUE)

knn_model <- train(medv ~ ., #full model
                   data = train_data,
                   method = "knn",
                   metric = "RMSE",
                   trControl = ctrl,
                   tuneLength = 5)

tree_model <- train(medv ~ ., #full model
                   data = train_data,
                   method = "rpart",
                   metric = "RMSE",
                   trControl = ctrl,
                   tuneLength = 5)

lm_model <- train(medv ~ ., #full model
                    data = train_data,
                    method = "lm",
                    metric = "RMSE",
                    trControl = ctrl,
                    tuneLength = 5)

ranger_model <- train(medv ~ ., #full model
                  data = train_data,
                  method = "ranger",
                  metric = "RMSE",
                  trControl = ctrl,
                  tuneLength = 5)

#3.score (prediction)
knn <- predict(knn_model, newdata = test_data)
tree <- predict(tree_model, newdata = test_data)
lm <- predict(lm_model, newdata = test_data)
ranger <- predict(ranger_model, newdata = test_data)

#4.evaluate model performance
knn_rmse <- sqrt(mean((knn - test_data$medv)**2))
tree_rmse <- sqrt(mean((tree - test_data$medv)**2))
lm_rmse <- sqrt(mean((lm - test_data$medv)**2))
ranger_rmse <- sqrt(mean((ranger - test_data$medv)**2))
