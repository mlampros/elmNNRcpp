#include <R.h>
#include <Rinternals.h>
#include <stdlib.h> // for NULL
#include <R_ext/Rdynload.h>

/* FIXME: 
   Check these declarations against the C/Fortran source code.
*/

/* .Call calls */
extern SEXP _elmNNRcpp_activation_functions(SEXP, SEXP, SEXP);
extern SEXP _elmNNRcpp_elm_predict_rcpp(SEXP, SEXP, SEXP);
extern SEXP _elmNNRcpp_elm_train_rcpp(SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP);
extern SEXP _elmNNRcpp_hardlim(SEXP);
extern SEXP _elmNNRcpp_hardlims(SEXP);
extern SEXP _elmNNRcpp_norm_preds(SEXP);
extern SEXP _elmNNRcpp_onehot_labels_rcpp(SEXP);
extern SEXP _elmNNRcpp_relu(SEXP, SEXP);
extern SEXP _elmNNRcpp_satlins(SEXP);
extern SEXP _elmNNRcpp_set_seed(SEXP);
extern SEXP _elmNNRcpp_tribas(SEXP);
extern SEXP _elmNNRcpp_uniform_negative(SEXP, SEXP);

static const R_CallMethodDef CallEntries[] = {
    {"_elmNNRcpp_activation_functions", (DL_FUNC) &_elmNNRcpp_activation_functions,  3},
    {"_elmNNRcpp_elm_predict_rcpp",     (DL_FUNC) &_elmNNRcpp_elm_predict_rcpp,      3},
    {"_elmNNRcpp_elm_train_rcpp",       (DL_FUNC) &_elmNNRcpp_elm_train_rcpp,       10},
    {"_elmNNRcpp_hardlim",              (DL_FUNC) &_elmNNRcpp_hardlim,               1},
    {"_elmNNRcpp_hardlims",             (DL_FUNC) &_elmNNRcpp_hardlims,              1},
    {"_elmNNRcpp_norm_preds",           (DL_FUNC) &_elmNNRcpp_norm_preds,            1},
    {"_elmNNRcpp_onehot_labels_rcpp",   (DL_FUNC) &_elmNNRcpp_onehot_labels_rcpp,    1},
    {"_elmNNRcpp_relu",                 (DL_FUNC) &_elmNNRcpp_relu,                  2},
    {"_elmNNRcpp_satlins",              (DL_FUNC) &_elmNNRcpp_satlins,               1},
    {"_elmNNRcpp_set_seed",             (DL_FUNC) &_elmNNRcpp_set_seed,              1},
    {"_elmNNRcpp_tribas",               (DL_FUNC) &_elmNNRcpp_tribas,                1},
    {"_elmNNRcpp_uniform_negative",     (DL_FUNC) &_elmNNRcpp_uniform_negative,      2},
    {NULL, NULL, 0}
};

void R_init_elmNNRcpp(DllInfo *dll)
{
    R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
    R_useDynamicSymbols(dll, FALSE);
}
