# include <RcppArmadillo.h>
// [[Rcpp::depends("RcppArmadillo")]]
// [[Rcpp::plugins(openmp)]]
// [[Rcpp::plugins(cpp11)]]


#ifdef _OPENMP
#include <omp.h>
#endif

// RAND_MAX (used in 'uniform_negative') is a constant defined in <cstdlib> ( see: http://www.cplusplus.com/reference/cstdlib/rand/ )
#include <cstdlib>


// 'satlins' activation function
//

// [[Rcpp::export]]
arma::mat satlins(arma::mat x) {

  arma::uvec idx_greater = arma::find(x >= 1.0);

  arma::uvec idx_less = arma::find(x <= -1.0);

  if (idx_greater.n_elem > 0) {

    x(idx_greater).fill(1.0);
  }

  else {

    if (idx_less.n_elem > 0) {

      x(idx_less).fill(-1.0);
    }
  }

  return x;
}



// 'tribas' activation function
//

// [[Rcpp::export]]
arma::mat tribas(arma::mat x) {

  arma::uvec idx_condition = arma::find(x >= -1.0 && x <= 1.0);

  arma::uvec idx_NOT_condition = arma::find(x < -1.0 && x > 1.0);

  if (idx_condition.n_elem > 0) {

    for (unsigned int i = 0; i < idx_condition.n_elem; i++) {

      x(idx_condition(i)) = 1.0 - std::abs(x(idx_condition(i)));
    }
  }

  if (idx_NOT_condition.n_elem > 0) {

    x(idx_NOT_condition).fill(0.0);
  }

  return x;
}



// 'hardlim' activation function
//

// [[Rcpp::export]]
arma::mat hardlim(arma::mat x) {

  arma::uvec idx_greater = arma::find(x >= 0.0);

  arma::uvec idx_less = arma::find(x < 0.0);

  if (idx_greater.n_elem > 0) {

    x(idx_greater).fill(1.0);
  }

  if (idx_less.n_elem > 0) {

    x(idx_less).fill(0.0);
  }

  return x;
}



// 'hardlims' activation function
//

// [[Rcpp::export]]
arma::mat hardlims(arma::mat x) {

  arma::uvec idx_greater = arma::find(x >= 0.0);

  arma::uvec idx_less = arma::find(x < 0.0);

  if (idx_greater.n_elem > 0) {

    x(idx_greater).fill(1.0);
  }

  if (idx_less.n_elem > 0) {

    x(idx_less).fill(-1.0);
  }

  return x;
}



// 'relu' activation function
//[ I renamed 'poslin' to 'relu' because it's easier to remember I also added alpha for leaky-relu ]
//

// [[Rcpp::export]]
arma::mat relu(arma::mat x, double alpha = 0.0) {

  arma::uvec idx = arma::find(x < 0.0);

  if (idx.n_elem > 0) {

    if (alpha == 0.0) {

      x(idx).fill(0.0);
    }

    else {

      x(idx) *= alpha;
    }
  }

  return x;
}



// use base R's set.seed() in Rcpp for RNG
// http://thecoatlessprofessor.com/programming/set_rs_seed_in_rcpp_sequential_case
//

// [[Rcpp::export]]
void set_seed(int seed) {

  Rcpp::Environment base_env("package:base");

  Rcpp::Function set_seed_r = base_env["set.seed"];

  set_seed_r(seed);
}



// uniform negative [-1, 1] (see my commented out scipt below)
// see issue: https://github.com/mlampros/elmNNRcpp/issues/2
// slightly modified from https://stackoverflow.com/a/34757263/8302386
//

// [[Rcpp::export]]
arma::mat uniform_negative(int n_rows, int n_cols) {

  arma::mat res_neg = arma::randu(n_rows, n_cols);
  res_neg *= RAND_MAX;
  res_neg = -1.0 + res_neg / (RAND_MAX / 2.0);
  return res_neg;
}



// //-------------------------------------------------------------------------------------------------------  No effect, because I set the seed inside the 'elm_train_rcpp' function, see issue: https://github.com/mlampros/elmNNRcpp/issues/2  [ keep as a reference for negative-uniform-distribution ]
// // random-uniform-matrix generation [ I can adjust the cols, rows, min-value, max-value ]
// // [ see the Armadillo documentation for more details : http://arma.sourceforge.net/docs.html ]
// //
//
// // [[Rcpp::export]]
// arma::mat randomMatrix(int nCols, int nRows, double minValue, double maxValue) {
//
//   std::mt19937 engine;  // Mersenne twister random number engine
//
//   std::uniform_real_distribution<double> distr(minValue, maxValue);
//
//   arma::mat A(nRows, nCols);
//
//   A.imbue( [&]() { return distr(engine); } );
//
//   return A;
// }
//
//
// // uniform value generation for a vector
// // [ see the Armadillo documentation for more details : http://arma.sourceforge.net/docs.html ]
// //
//
// // [[Rcpp::export]]
// std::vector<double> uniform_vector(int iters, double minValue, double maxValue) {
//
//   std::mt19937 engine;  // Mersenne twister random number engine
//
//   std::uniform_real_distribution<double> distr(minValue, maxValue);
//
//   std::vector<double> res;
//
//   for (int i = 0; i < iters; i++) {
//
//     res.push_back(distr(engine));
//   }
//
//   return res;
// }
// //-------------------------------------------------------------------------------------------------------



// switch function for activation functions
//

// [[Rcpp::export]]
arma::mat activation_functions(arma::mat& tempH, std::string actfun, double alpha = 0.0) {

  arma::mat H;

  if (actfun == "sig") { H = 1.0 / (1.0 + arma::exp(-1.0 * tempH)); }

  else if (actfun == "sin") { H = arma::sin(tempH); }

  else if (actfun == "radbas") { H = arma::exp(-1.0 * (arma::pow(tempH, 2.0))); }

  else if (actfun == "hardlim") { H = hardlim(tempH); }

  else if (actfun == "hardlims")  { H = hardlims(tempH); }

  else if (actfun == "satlins") { H = satlins(tempH); }

  else if (actfun == "tansig") { H = (2.0 / ( 1 + arma::exp(-2.0 * tempH)) - 1.0); }

  else if (actfun == "tribas") { H = tribas(tempH); }

  else if (actfun == "relu") { H = relu(tempH, alpha); }

  else if (actfun == "purelin") { H = tempH; }

  else {

    std::string message = "ERROR: " + actfun + " is not a valid activation function.";

    Rcpp::stop(message);
  }

  return H;
}




// extreme learning machines train-function     [ init_weights can be either "normal_gaussian" ([0,1]), "uniform_positive" ([0,1]) or "uniform_negative" ([-1,1]) ]
//

// [[Rcpp::export]]
Rcpp::List elm_train_rcpp(arma::mat& x, arma::mat y, int nhid, std::string actfun, std::string init_weights = "normal_gaussian", bool bias = false,

                          double moorep_pseudoinv_tol = 0.01, double alpha = 0.0, int seed = 1, bool verbose = false) {


  if (nhid < 1) Rcpp::Rcout << "ERROR: number of hidden neurons must be >= 1" << std::endl;

  int transpose_ROWS = x.n_rows;

  int transpose_COLS = x.n_cols;

  arma::mat inpweight;

  arma::colvec biashid;

  set_seed(seed);

  if (verbose) Rcpp::Rcout << "Input weights will be initialized ..." << std::endl;

  if (init_weights == "normal_gaussian") {

    inpweight = arma::randn<arma::mat>(nhid, transpose_COLS);

    if (bias) {

      biashid = arma::randn<arma::colvec>(nhid);
    }
  }

  else if (init_weights == "uniform_positive") {

    inpweight = arma::randu<arma::mat>(nhid, transpose_COLS);

    if (bias) {

      biashid = arma::randu<arma::colvec>(nhid);
    }
  }

  else if (init_weights == "uniform_negative") {

    inpweight = uniform_negative(nhid, transpose_COLS);

    if (bias) {

      biashid = arma::conv_to< arma::colvec >::from(uniform_negative(nhid, 1));
    }
  }

  else {

    Rcpp::stop("Invalid type for the 'init_weights' parameter");
  }

  if (verbose) Rcpp::Rcout << "Dot product of input weights and data starts ..." << std::endl;

  x = inpweight * x.t();

  arma::mat biasMatrix;

  if (bias) {

    if (verbose) Rcpp::Rcout << "Bias will be added to the dot product ..." << std::endl;

    biasMatrix.set_size(biashid.size(), transpose_ROWS);

    for (int i = 0; i < transpose_ROWS; i++) {

      biasMatrix.col(i) = biashid;
    }

    x += biasMatrix;
  }

  if (verbose) Rcpp::Rcout << "'" + actfun + "' activation function will be utilized ..." << std::endl;

  x = activation_functions(x, actfun, alpha);

  x = x.t();

  if (verbose) Rcpp::Rcout << "The computation of the Moore-Pseudo-inverse starts ..." << std::endl;

  arma::mat outweight = arma::pinv(x, moorep_pseudoinv_tol, "dc" );

  if (outweight.n_cols != y.n_rows) { Rcpp::stop("The number of rows of the input data and the size of the response variable do not match!"); }

  outweight = outweight * y;

  arma::mat Y = x * outweight;

  arma::mat residuals = y - Y;

  if (verbose) Rcpp::Rcout << "The computation is finished!" << std::endl;

  return Rcpp::List::create(Rcpp::Named("inpweight") = inpweight, Rcpp::Named("biashid") = biashid, Rcpp::Named("outweight") = outweight, Rcpp::Named("actfun") = actfun,

                            Rcpp::Named("nhid") = nhid, Rcpp::Named("predictions") = Y, Rcpp::Named("fitted_values") = Y, Rcpp::Named("residuals") = residuals);
}



// normalize predictions to [0,1] range
//

// [[Rcpp::export]]
arma::mat norm_preds(arma::mat x) {

  x = arma::abs(x);

  arma::colvec sum_x = sum(x, 1);

  for (unsigned int i = 0; i < sum_x.n_elem; i++) {

    x.row(i) /= sum_x(i);
  }

  return x;
}




// prediction function for elmNNRcpp       [ in case of classification it returns probabilities ]
//

// [[Rcpp::export]]
arma::mat elm_predict_rcpp(Rcpp::List object, arma::mat &newdata, bool normalize = false) {

  arma::mat inpweight = Rcpp::as<arma::mat>(object[0]);

  arma::colvec biashid = Rcpp::as<arma::colvec>(object[1]);

  arma::mat outweight = Rcpp::as<arma::mat>(object[2]);

  std::string actfun = Rcpp::as<std::string>(object[3]);

  int transpose_COLS = newdata.n_rows;

  // arma::mat TV_P = newdata.t();

  arma::mat tmpHTest = inpweight * newdata.t();

  if (!biashid.empty()) {

    arma::mat biasMatrixTE(biashid.n_elem, transpose_COLS);      // transpose_COLS == TV_P.n_cols

    for (int i = 0; i < transpose_COLS; i++) {          // transpose_COLS == TV_P.n_cols

      biasMatrixTE.col(i) = biashid;
    }

    tmpHTest += biasMatrixTE;
  }

  arma::mat HTest = activation_functions(tmpHTest, actfun);

  arma::mat predictions = HTest.t() * outweight;

  if (normalize) {

    if (predictions.n_cols > 1) {

      predictions = norm_preds(predictions);                   // normalize each row to [0,1] as it's possible that the output probabilities include negative values
    }
  }

  return predictions;
}



// build one-hot labels from a numeric vector
// it expects the labels to be in the range between [0,n]
//

// [[Rcpp::export]]
arma::mat onehot_labels_rcpp(arma::rowvec x) {

  arma::rowvec unq = arma::unique(x);

  if (arma::min(unq) != 0) {

    Rcpp::stop("The minimum value for the unique labels (response variable) should be 0!");
  }

  arma::mat onehot_res(x.n_elem, unq.n_elem, arma::fill::zeros);

  for (unsigned int i = 0; i < x.n_elem; i++) {

    onehot_res(i, x(i)) = 1;
  }

  return onehot_res;
}


