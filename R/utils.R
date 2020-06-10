

#' Extreme Learning Machine training function
#'
#'
#' @param x a matrix. The columns of the input matrix should be of type numeric
#' @param y a matrix. In case of regression the matrix should have \emph{n} rows and \emph{1} column. In case of classification it should consist of \emph{n} rows and \emph{n} columns, where \emph{n > 1} and equals to the number of the unique labels.
#' @param nhid a numeric value specifying the hidden neurons. Must be >= 1
#' @param actfun a character string specifying the type of activation function. It should be one of the following : 'sig' \emph{( sigmoid )}, 'sin' \emph{( sine )}, 'radbas' \emph{( radial basis )}, 'hardlim' \emph{( hard-limit )}, 'hardlims' \emph{( symmetric hard-limit )}, 'satlins' \emph{( satlins )}, 'tansig' \emph{( tan-sigmoid )}, 'tribas' \emph{( triangular basis )}, 'relu' \emph{( rectifier linear unit )} or 'purelin' \emph{( linear )}
#' @param init_weights a character string spcecifying the distribution from which the \emph{input-weights} and the \emph{bias} should be initialized. It should be one of the following : 'normal_gaussian' \emph{(normal / Gaussian distribution with zero mean and unit variance)}, 'uniform_positive' \emph{( in the range [0,1] )} or 'uniform_negative' \emph{( in the range [-1,1] )}
#' @param bias either TRUE or FALSE. If TRUE then \emph{bias} weights will be added to the hidden layer
#' @param moorep_pseudoinv_tol a numeric value. See the references web-link for more details on \emph{Moore-Penrose pseudo-inverse} and specifically on the \emph{pseudo inverse tolerance value}
#' @param leaky_relu_alpha a numeric value between 0.0 and 1.0. If 0.0 then a simple \emph{relu} ( f(x) = 0.0 for x < 0, f(x) = x for x >= 0 ) activation function will be used, otherwise a \emph{leaky-relu} ( f(x) = alpha * x for x < 0, f(x) = x for x >= 0 ). It is applicable only if \emph{actfun} equals to 'relu'
#' @param seed a numeric value specifying the random seed. Defaults to 1
#' @param verbose a boolean. If TRUE then information will be printed in the console
#'
#' @details
#'
#' The input matrix should be of type numeric. This means the user should convert any \emph{character}, \emph{factor} or \emph{boolean} columns to numeric values before using the \emph{elm_train} function
#'
#' @references
#'
#' http://arma.sourceforge.net/docs.html
#'
#' https://en.wikipedia.org/wiki/Moore%E2%80%93Penrose_inverse
#'
#' https://www.kaggle.com/robertbm/extreme-learning-machine-example
#'
#' http://rt.dgyblog.com/ml/ml-elm.html
#'
#' @export
#' @examples
#'
#' library(elmNNRcpp)
#'
#' #-----------
#' # Regression
#' #-----------
#'
#' data(Boston, package = 'KernelKnn')
#'
#' Boston = as.matrix(Boston)
#' dimnames(Boston) = NULL
#'
#' x = Boston[, -ncol(Boston)]
#' y = matrix(Boston[, ncol(Boston)], nrow = length(Boston[, ncol(Boston)]), ncol = 1)
#'
#' out_regr = elm_train(x, y, nhid = 20, actfun = 'purelin', init_weights = 'uniform_negative')
#'
#'
#' #---------------
#' # Classification
#' #---------------
#'
#' data(ionosphere, package = 'KernelKnn')
#'
#' x_class = ionosphere[, -c(2, ncol(ionosphere))]
#' x_class = as.matrix(x_class)
#' dimnames(x_class) = NULL
#'
#' y_class = as.numeric(ionosphere[, ncol(ionosphere)])
#'
#' y_class_onehot = onehot_encode(y_class - 1)     # class labels should begin from 0
#'
#' out_class = elm_train(x_class, y_class_onehot, nhid = 20, actfun = 'relu')
#'

elm_train = function(x, y, nhid, actfun, init_weights = "normal_gaussian", bias = FALSE, moorep_pseudoinv_tol = 0.01,

                     leaky_relu_alpha = 0.0, seed = 1, verbose = FALSE) {

  if (verbose) START = Sys.time()

  if (!inherits(x, "matrix")) stop("The 'x' parameter should be of type 'matrix'", call. = F)
  if (!inherits(y, "matrix")) stop("The 'y' parameter should be of type 'matrix'", call. = F)

  res = elm_train_rcpp(x, y, nhid, actfun, init_weights, bias, moorep_pseudoinv_tol, leaky_relu_alpha, seed, verbose)

  if (verbose) {

    END = Sys.time()

    t = END - START

    cat('\n'); cat('Time to complete :', t, attr(t, 'units'), '\n');
  }

  return(res)
}




#' Extreme Learning Machine predict function
#'
#'
#' @param elm_train_object it should be the output of the \emph{elm_train} function
#' @param newdata an input matrix with number of columns equal to the \emph{x} parameter of the \emph{elm_train} function
#' @param normalize a boolean specifying if the output predictions \emph{in case of classification} should be normalized. If TRUE then the values of each row of the output-probability-matrix that are less than 0 and greater than 1 will be pushed to the [0,1] range
#'
#' @export
#' @examples
#'
#' library(elmNNRcpp)
#'
#' #-----------
#' # Regression
#' #-----------
#'
#' data(Boston, package = 'KernelKnn')
#'
#' Boston = as.matrix(Boston)
#' dimnames(Boston) = NULL
#'
#' x = Boston[, -ncol(Boston)]
#' y = matrix(Boston[, ncol(Boston)], nrow = length(Boston[, ncol(Boston)]), ncol = 1)
#'
#' out_regr = elm_train(x, y, nhid = 20, actfun = 'purelin', init_weights = 'uniform_negative')
#'
#' pr_regr = elm_predict(out_regr, x)
#'
#'
#' #---------------
#' # Classification
#' #---------------
#'
#' data(ionosphere, package = 'KernelKnn')
#'
#' x_class = ionosphere[, -c(2, ncol(ionosphere))]
#' x_class = as.matrix(x_class)
#' dimnames(x_class) = NULL
#'
#' y_class = as.numeric(ionosphere[, ncol(ionosphere)])
#'
#' y_class_onehot = onehot_encode(y_class - 1)     # class labels should begin from 0
#'
#' out_class = elm_train(x_class, y_class_onehot, nhid = 20, actfun = 'relu')
#'
#' pr_class = elm_predict(out_class, x_class, normalize = TRUE)
#'

elm_predict = function(elm_train_object, newdata, normalize = FALSE) {

  if (!inherits(newdata, "matrix")) stop("The 'newdata' parameter should be of type 'matrix'", call. = F)

  res = elm_predict_rcpp(elm_train_object, newdata, normalize)

  return(res)
}




#' One-hot-encoding of the labels in case of classification
#'
#'
#' @param y a numeric vector consisting of the response variable labels. The minimum value of the unique labels should begin from 0
#'
#' @export
#' @examples
#'
#' library(elmNNRcpp)
#'
#' y = sample(0:3, 100, replace = TRUE)
#'
#' y_expand = onehot_encode(y)
#'

onehot_encode = function(y) {

  DIF = diff(sort(unique(y)))

  if (!all(DIF == 1)) stop("The difference between the unique labels is greater than 1. Make sure that the unique labels look, for instance, like the following : c(0, 1, 2) and NOT like c(0, 2, 3), where the difference between consecutive labels is greater than 1", call. = F)

  res = onehot_labels_rcpp(y)

  return(res)
}

