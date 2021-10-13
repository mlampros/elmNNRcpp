#' Fit an extreme learning model
#'
#' Formula interface for \code{\link{elm_train}}, transforms a data frame and formula
#' into the necessary input for elm_train, automatically calls \code{\link{onehot_encode}}
#' for classification.
#'
#' @examples
#' elm(Species ~ ., data = iris, nhid = 20, actfun="sig")
#' 
#' mod_elm <- elm(Species ~ ., data = iris, nhid = 20, actfun="sig")
#' 
#' # predict classes
#' predict(mod_elm, newdata = iris[1:3,-5])
#' 
#' # predict probabilities
#' predict(mod_elm, newdata = iris[1:3,-5], type="prob")
#'
#' # predict elm output
#' predict(mod_elm, newdata = iris[1:3,-5], type="output")
#' 
#' data("Boston")
#' elm(medv ~ ., data = Boston, nhid = 40, actfun="relu")
#'
#' data("ionosphere")
#' elm(class ~ ., data = ionosphere, nhid=20, actfun="relu")
#'
#' @export
#' @inheritParams elm_train
#' @param f formula used to specify the regression or classification.
#' @param data data.frame with the data
#' @return elm object which can be used with predict, residuals and fitted.
elm <- function(formula, data, nhid, actfun, init_weights = "normal_gaussian", bias = FALSE, moorep_pseudoinv_tol = 0.01,
                leaky_relu_alpha = 0.0, seed = 1, verbose = FALSE){
  data <- as.data.frame(data)
  mf <- stats::model.frame(formula, data = data)
  mm <- stats::model.matrix(formula, mf)

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

  fit$formula <- stats::terms(mf)
  fit$call <- sys.call()
  fit$nhid <- nhid
  fit$actfun <- actfun
  fit$is_regression <- is_regression

  colnames(fit$inpweight) <- colnames(x)

  if (is_regression){
    #dim(fit$outweight) <- NULL
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
    cat("\nconfusion matrix :\n")
    print(table(observed=x$y, predicted = x$pred_class))
  }
}

#' @export
residuals.elm <- function(object, ...){
  object$residuals
}

#' @export
fitted.elm <- function(object, ...){
  object$fitted_values
}

#' @export
#' @rdname elm
#' @param object elm model fitted with \code{\link{elm}}.
#' @param newdata data.frame with the new data
#' @param type only used with classification, can be either "class", "prob", "output", 
#' which are class (vector), probability (matrix) or the output of the elm function (matrix).
#' @param ... not used
predict.elm <- function(object, newdata, type=c("class", "prob", "output"), ...){
  type <- match.arg(type)
  if (object$is_regression && type != "class"){
    warning("type is ignored for regression", call. = FALSE)
  }
  if (missing(newdata)){
    predictions <- object$predictions
  } else {
    f <- object$formula
    
    y_name <- as.character(f[[2]])
    newdata[y_name] <- 1 # just a value, not used

    mf <- stats::model.frame(object$formula, data = newdata)
    mm <- stats::model.matrix(f, mf)
    x <- mm[,-1]
    
    object$outweight
    predictions <- elm_predict(unclass(object), newdata = x, normalize = (type=="prob"))
  }
  
  if (!object$is_regression && type == "class"){
    levs <- colnames(object$predictions)
    pred_class <- levs[apply(predictions, 1, which.max)]
    predictions <- factor(pred_class, levels=levs)
  }
  
  drop(predictions)
}
