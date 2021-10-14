test_that("elm regression works", {
  boston_train <- KernelKnn::Boston[1:350,]
  boston_test <- KernelKnn::Boston[-c(1:350),]
  
  model_train <- elm_train(xtr, ytr, nhid=100, actfun = "sig")
  model <- elm(medv ~ ., data = boston_train, nhid=100, actfun="sig")
  
  expect_equal(class(model), "elm")
  expect_true(model$is_regression)
  
  pred <- predict(model, newdata=boston_train)
  expect_equal(pred, model$predictions)
  expect_equal(residuals(model), model$residuals)
  expect_equal(fitted(model), model$fitted_values)
  
  expect_equal(model$outweight, model_train$outweight)
  expect_equal(model$predictions, drop(model_train$predictions))
  expect_equal(model$residuals, drop(model_train$residuals))
})

test_that("elm classification works", {
  ionosphere_train <- KernelKnn::ionosphere[1:200, -2]
  ionosphere_test <- KernelKnn::ionosphere[-c(1:200), -2]
  
  model_train <- elm_train(xtr_class, ytr_class, nhid=20, actfun = "relu")
  model <- elm(class ~ ., data = ionosphere_train, nhid=20, actfun="relu")
  
  expect_equal(class(model), "elm")
  expect_false(model$is_regression)
  
  pred <- predict(model, newdata=ionosphere_train, type="raw")
  pred_train <- elm_predict(model_train, newdata = xtr_class, normalize = FALSE)
  expect_equal(pred, model$predictions)
  expect_equal(unname(pred), model_train$predictions)
  expect_equal(unname(pred), pred_train)
  
  pred <- predict(model, newdata=ionosphere_train, type="prob")
  pred_train <- elm_predict(model_train, newdata = xtr_class, normalize = TRUE)
  expect_equal(unname(pred), pred_train)
  
  pred <- predict(model, newdata = ionosphere_train, type="class")
  expect_equal(pred, model$pred_class)
  
  expect_equal(residuals(model), model$residuals)
  expect_equal(fitted(model), model$fitted_values)
  
  expect_equal(unname(model$outweight), model_train$outweight)
  expect_equal(unname(model$predictions), drop(model_train$predictions))
  expect_equal(unname(model$residuals), drop(model_train$residuals))
  
  expect_equal(colnames(model$outweight), levels(ionosphere$class))
  expect_equal(colnames(model$predictions), levels(ionosphere$class))
  expect_equal(colnames(model$residuals), levels(ionosphere$class))
})

test_that("elm works with binary classification", {
  data <- data.frame(y = c(TRUE, FALSE), x = 1:2)
  model <- elm(y ~ ., data = data, nhid=1, actfun = "sig")
  expect_equal(model$pred_class, data$y)
  
  pred <- predict(model, newdata = data)
  expect_equal(pred, model$pred_class)
  
  pred <- predict(model, newdata = data, type="raw")
  expect_equal(pred, model$predictions)
})
