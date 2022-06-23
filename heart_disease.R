#load library
library(tidyverse)
library(glue)
library(stringr)
library(caret)
library(randomForest)
library(ROSE)
library(e1071)

#load data set
h <- read_csv('~/Work/R/HeartDisease/heart_disease_2020.csv')

# check missing values
if (mean(complete.cases(h)) == 1) print("No NA found")

#review data
glimpse(h)

#descriptive summary
summary(h)

h %>%
  count(HeartDisease) %>%
  mutate(percenttage = n/sum(n))

#Undersampling by Group using the ROSE Package
h_balanced = NULL


for (n in unique(h$HeartDisease)) {
  tmp<-ovun.sample(HeartDisease ~ ., data = h, method = "under", p = 0.5, seed = 27)$data
  h_balanced<-rbind(h_balanced, tmp)
}

h_balanced %>%
  count(HeartDisease) %>%
  mutate(percenttage = n/sum(n))


#1.split data into 80% train and 20% test
set.seed(27)
n <- nrow(h_balanced)
train_size <- 0.8
train_id <- sample(1:n, size= train_size*n, replace=FALSE)

train_data <- h_balanced[train_id, ]
test_data <- h_balanced[-train_id, ]

# check n for each partition
print(nrow(train_data)) 
print(nrow(test_data))


#2.train model
set.seed(27)
ctrl <- trainControl(method = "repeatedcv",
                     number = 5,
                     repeats = 5,
                     verboseIter = TRUE)

knn_model <- train(HeartDisease ~ .,
                   data = train_data,
                   method = "knn",
                   metric = "Accuracy",
                   trControl = ctrl)

tree_model <- train(HeartDisease ~ .,
                    data = train_data,
                    method = "rpart",
                    metric = "Accuracy",
                    trControl = ctrl)

randomForrest_model <- train(HeartDisease ~ .,
                             data = train_data,
                             method = "rf",
                             metric = "Accuracy",
                             trControl = ctrl)

#3.score (prediction)
knn <- predict(knn_model, newdata = test_data)
tree <- predict(tree_model, newdata = test_data)
randomForrest <- predict(randomForrest_model, newdata = test_data)

#4.evaluate model performance
knn_rmse <- mean(knn == test_data$HeartDisease)
tree_rmse <- mean(tree == test_data$HeartDisease)
randomForrest_rmse <- mean(randomForrest == test_data$HeartDisease)

#5.model comparison
modelList <- list(
  kNN = knn_model,
  dicissionTree = tree_model,
  rdmForrest = randomForrest_model
)

result <- resamples(modelList)
summary(result)

#confusion matrix
#check class reference always
confusionMatrix(randomForrest, 
                as.factor(test_data$HeartDisease), 
                mode = "prec_recall",
                positive = "Yes")

#check importance variable
varImp(randomForrest_model)
