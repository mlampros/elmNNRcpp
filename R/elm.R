#' elm model
#'
#' Formula interface for elm_train, transforms a data frame and formula
#' into the necessary input for elm_train.
#'
#' @examples
#' elm(Species ~ ., data = iris, nhid = 20, actfun="sig")
#'
#' data("Boston")
#' elm(medv ~ ., data = Boston, nhid = 40, actfun="relu")
#'
#'
#' data("ionosphere")
#' elm(class ~ ., data = ionosphere, nhid=20, actfun="relu")
#'
#' @inheritParams elm_train
#' @param f formula used to specify the regresion / classification
#' @param data data.frame with the data
#' @return elm object which can be used with predict, residuals and fitted.
elm <- function(f, data, nhid, actfun, init_weights = "normal_gaussian", bias = FALSE, moorep_pseudoinv_tol = 0.01,
                leaky_relu_alpha = 0.0, seed = 1, verbose = FALSE){
  data <- as.data.frame(data)
  mf <- stats::model.frame(f, data = data)
  mm <- stats::model.matrix(f, mf)

  y <- mf[[1]]

  # TODO fix the categorical predictors...
  x <- mm[,-1]

  # regression or classification?
  is_regression <- is.numeric(y)

  if (is_regression){
    y <- matrix(y, ncol=1)
  } else {
    # TODO logical
    yf <- as.factor(y)

    levs <- levels(yf)
    y <- onehot_encode(as.integer(yf) - 1)
    colnames(y) <- levs
  }

  fit <- elm_train( x = x
                  , y = y
                  , nhid = nhid
                  , actfun = actfun
                  , init_weights = init_weights
                  , bias =bias
                  , moorep_pseudoinv_tol = moorep_pseudoinv_tol
                  , leaky_relu_alpha = leaky_relu_alpha
                  , seed = seed
                  , verbose = verbose
                  )

  class(fit) <- "elm"

  fit$formula <- terms(mf)
  fit$call <- sys.call()
  fit$nhid <- nhid
  fit$actfun <- actfun
  fit$is_regression <- is_regression

  colnames(fit$inpweight) <- colnames(x)

  if (is_regression){
    dim(fit$outweight) <- NULL
    dim(fit$predictions) <- NULL
    dim(fit$fitted_values) <- NULL
    dim(fit$residuals) <- NULL
  } else {
    colnames(fit$outweight) <- levs
    colnames(fit$predictions) <- levs
    colnames(fit$fitted_values) <- levs
    colnames(fit$residuals) <- levs
    fit$pred_class <- levs[apply(fit$predictions, 1, which.max)]
    fit$pred_class <- factor(fit$pred_class, levels=levs)
    fit$y <- yf
  }

  fit
}


#' @export
print.elm <- function(x,...){
  cat("Extreme learning model, elm")
  if (x$is_regression){
    cat(" (regression)")
  } else {
    cat(" (classification)")
  }
  cat(":\n\n")
  cat("call: ", deparse(x$call), "\n")
  cat("hidden units       :", x$nhid, "\n")
  cat("activation function:", x$actfun, "\n")

  if (x$is_regression){
    cat("mse                :", mean(x$residuals^2), "\n")
  } else {
    cat("accuracy           :", mean(x$y == x$pred_class), "\n")
    cat("\nconfusion matrix:\n")
    print(table(observed=x$y, predicted = x$pred_class))
  }
  #print(unclass(x))
}

residuals.elm <- function(object, ...){
  object$residuals
}

fitted.elm <- function(object, ...){
  object$fitted_values
}

predict.elm <- function(object, newdata, normalize=FALSE, ...){
  if (missing(newdata)){
    predictions <- object$predictions
  } else {
    f <- object$formula

    y_name <- as.character(f[[2]])
    newdata[y_name] <- 1

    mf <- stats::model.frame(object$formula, data = newdata)

    x <- as.matrix(mf[-1])
    predictions <- elm_predict(object, newdata = x, normalize = normalize)
  }
  predictions
}
