library(randomForest)
library(rpart)
library(caret)


model1_acc <- function(train)
{
  model <- lm(target~., data=train)
  
  train_pred <- predict(model, train)
  train_acc <- mean((train_pred - train$target) ^2) ^0.5
  
  return(train_acc)
}

model2_acc <- function(train)
{
  model <- glm(target~., data=train)

  train_pred <- predict(model, train)
  train_acc <- mean((train_pred - train$target) ^2) ^0.5

  return(train_acc)
}

model3_acc <- function(train)
{
  model <- rpart(target~., method="anova", data=train)

  train_pred <- predict(model, train)
  train_acc <- mean((train_pred - train$target) ^2) ^0.5

  return(train_acc)
}

model4_acc <- function(train)
{
  fitControl <- trainControl(method="repeatedcv", number=2, repeats=1)
  model <- train(target~., data=train, method="gbm", trControl=fitControl, verbose=FALSE)
  
  train_pred <- predict(model, train)
  train_acc <- mean((train_pred - train$target) ^2) ^0.5
  
  return(train_acc)
}