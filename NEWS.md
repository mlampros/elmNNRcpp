
## elmNNRcpp 1.0.5

* I updated the `Makevars` and `Makevars.win` files by adding `-DARMA_USE_CURRENT` (see issue: https://github.com/RcppCore/RcppArmadillo/issues/476)
* I removed the `-mthreads` compilation option from the "Makevars.win" file
* I removed the "CXX_STD = CXX11" from the "Makevars" files, and the "[[Rcpp::plugins(cpp11)]]" from the "utils.cpp" file due to the following NOTE from CRAN, "NOTE Specified C++11: please drop specification unless essential" (see also: https://www.tidyverse.org/blog/2023/03/cran-checks-compiled-code/#note-regarding-systemrequirements-c11)


## elmNNRcpp 1.0.4

* A formula interface was added through function `elm` (see https://github.com/mlampros/elmNNRcpp/pull/4)


## elmNNRcpp 1.0.3

* I've added the *CITATION* file in the *inst* directory listing all papers and software used in the *elmNNRcpp* package


## elmNNRcpp 1.0.2

* I fixed the documentation of the *'normal_gaussian'* weights (see issue: *https://github.com/mlampros/elmNNRcpp/issues/1*)
* I fixed the *'uniform_negative'* distribution (see issue: *https://github.com/mlampros/elmNNRcpp/issues/2*)


## elmNNRcpp 1.0.1

I added the *Github repository Url* and the *BugReports Url* in the DESCRIPTION file.


## elmNNRcpp 1.0.0

