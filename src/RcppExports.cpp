// This file was generated by Rcpp::compileAttributes
// Generator token: 10BE3573-1514-4C36-9D1C-5A225CD40393

#include <Rcpp.h>

using namespace Rcpp;

// compute_osi
Rcpp::DataFrame compute_osi(Rcpp::DataFrame df);
RcppExport SEXP icueval_compute_osi(SEXP dfSEXP) {
BEGIN_RCPP
    Rcpp::RObject __result;
    Rcpp::RNGScope __rngScope;
    Rcpp::traits::input_parameter< Rcpp::DataFrame >::type df(dfSEXP);
    __result = Rcpp::wrap(compute_osi(df));
    return __result;
END_RCPP
}
