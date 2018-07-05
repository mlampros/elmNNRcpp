
context("elmNNRcpp tests")


#=============================
# 'elm_train' && 'elm_predict'
#=============================


testthat::test_that("the elm_train function returns an error if the x parameter is not a matrix", {

  testthat::expect_error( elm_train(df_bst, ytr, nhid = 20, actfun = 'purelin') )
})


testthat::test_that("the elm_train function returns an error if the y parameter is not a matrix", {

  testthat::expect_error( elm_train(xtr, ytr_error, nhid = 20, actfun = 'purelin') )
})


testthat::test_that("the elm_predict function returns an error if the activation function is invalid", {

  testthat::expect_error( elm_train(xtr, ytr, nhid = 20, actfun = 'invalid') )
})


testthat::test_that("the elm_predict function returns an error if the init_weights parameter is invalid", {

  testthat::expect_error( elm_train(xtr, ytr, nhid = 20, actfun = 'relu', init_weights = 'invalid') )
})


testthat::test_that("the elm_predict function returns an error if the newdata parameter is not a matrix", {

  tmp_out = elm_train(xtr, ytr, nhid = 20, actfun = 'relu')

  testthat::expect_error( elm_predict(tmp_out, df_bst) )
})


testthat::test_that("the function returns the correct output in case of REGRESSION", {

  regr_types = c('relu', 'purelin')

  OBJECT = list()

  PREDS = list()

  for (TYPE in regr_types) {

    tmp_out = elm_train(xtr, ytr, nhid = 20, actfun = TYPE, init_weights = 'normal_gaussian', bias = T)

    OBJECT[[TYPE]] = tmp_out

    pr = elm_predict(tmp_out, xte)

    PREDS[[TYPE]] = pr
  }

  obj = all(unlist(lapply(OBJECT, function(x) inherits(x, "list"))))

  preds = all(unlist(lapply(PREDS, function(x) inherits(x, "matrix") && nrow(x) == nrow(xte))))

  testthat::expect_true( all(c(obj, preds)) )
})


testthat::test_that("the function returns the correct output in case of CLASSIFICATION", {

  class_types = c('sig', 'sin', 'radbas', 'hardlim', 'hardlims', 'satlins', 'tansig', 'tribas')

  OBJECT = list()

  PREDS = list()

  for (TYPE in class_types) {

    tmp_out = elm_train(xtr_class, ytr_class, nhid = 20, actfun = TYPE, init_weights = 'uniform_positive', bias = T)

    OBJECT[[TYPE]] = tmp_out

    pr = elm_predict(tmp_out, xte_class, normalize = T)

    PREDS[[TYPE]] = pr
  }

  obj = all(unlist(lapply(OBJECT, function(x) inherits(x, "list"))))

  preds = all(unlist(lapply(PREDS, function(x) inherits(x, "matrix") && nrow(x) == nrow(xte_class) && ncol(x) == ncol(ytr_class))))

  testthat::expect_true( all(c(obj, preds)) )
})


testthat::test_that("the function returns the correct output in case of REGRESSION with leaky-relu", {

  regr_types = c('relu')

  OBJECT = list()

  PREDS = list()

  for (TYPE in regr_types) {

    tmp_out = elm_train(xtr, ytr, nhid = 20, actfun = TYPE, init_weights = 'uniform_negative', bias = T, leaky_relu_alpha = 0.01, verbose = T)

    OBJECT[[TYPE]] = tmp_out

    pr = elm_predict(tmp_out, xte)

    PREDS[[TYPE]] = pr
  }

  obj = all(unlist(lapply(OBJECT, function(x) inherits(x, "list"))))

  preds = all(unlist(lapply(PREDS, function(x) inherits(x, "matrix") && nrow(x) == nrow(xte))))

  testthat::expect_true( all(c(obj, preds)) )
})



#================
# 'onehot_encode'
#================


testthat::test_that("the onehot_encode function returns an error if the difference between the unique labels of the y parameter is greater than 1", {

  y = sample(0:3, 10, replace = T)

  y[y == 2] = 4

  testthat::expect_error( onehot_encode(y) )
})


testthat::test_that("the onehot_encode function returns an error if the response variable labels do not begin from 0", {

  y = sample(1:3, 10, replace = T)

  testthat::expect_error( onehot_encode(y) )
})


testthat::test_that("the onehot_encode function returns the correct output", {

  y = sample(0:3, 100, replace = T)

  res = onehot_encode(y)

  testthat::expect_true( length(unique(y)) == ncol(res) )
})

