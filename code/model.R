library(randomForest)
library(rpart)
library(caret)


model1_loss <- function(train, test)
{
  model <- lm(target~., data=train)
  
  train_pred <- predict(model, train)
  train_acc <- mean((train_pred - train$target) ^2) ^0.5
  
  test_pred <- predict(model, test)
  test_acc <- mean((test_pred - test$target) ^2) ^0.5
  
  return(c(train_acc, test_acc))
}

model2_loss <- function(train, test)
{
  model <- glm(target~., data=train)

  train_pred <- predict(model, train)
  train_acc <- mean((train_pred - train$target) ^2) ^0.5

  test_pred <- predict(model, test)
  test_acc <- mean((test_pred - test$target) ^2) ^0.5
  
  return(c(train_acc, test_acc))
}

model3_loss <- function(train, test)
{
  model <- rpart(target~., method="anova", data=train)

  train_pred <- predict(model, train)
  train_acc <- mean((train_pred - train$target) ^2) ^0.5

  test_pred <- predict(model, test)
  test_acc <- mean((test_pred - test$target) ^2) ^0.5
  
  return(c(train_acc, test_acc))
}

model4_loss <- function(train, test)
{
  fitControl <- trainControl(method="repeatedcv", number=2, repeats=1)
  model <- train(target~., data=train, method="gbm", trControl=fitControl, verbose=FALSE)
  
  train_pred <- predict(model, train)
  train_acc <- mean((train_pred - train$target) ^2) ^0.5
  
  test_pred <- predict(model, test)
  test_acc <- mean((test_pred - test$target) ^2) ^0.5
  
  return(c(train_acc, test_acc))
}

final_model <- function(train, test)
{
  fitControl <- trainControl(method="repeatedcv", number=2, repeats=1)
  model <- train(target~., data=train, method="gbm", trControl=fitControl, verbose=FALSE)
  
  test_pred <- predict(model, test)
  
  return(test_pred)
}