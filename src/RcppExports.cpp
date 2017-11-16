// Generated by using Rcpp::compileAttributes() -> do not edit by hand
// Generator token: 10BE3573-1514-4C36-9D1C-5A225CD40393

#include <Rcpp.h>

using namespace Rcpp;

// mfrs_dag
List mfrs_dag(int node_count, IntegerVector composite_nodes);
RcppExport SEXP _rmfr_mfrs_dag(SEXP node_countSEXP, SEXP composite_nodesSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< int >::type node_count(node_countSEXP);
    Rcpp::traits::input_parameter< IntegerVector >::type composite_nodes(composite_nodesSEXP);
    rcpp_result_gen = Rcpp::wrap(mfrs_dag(node_count, composite_nodes));
    return rcpp_result_gen;
END_RCPP
}

static const R_CallMethodDef CallEntries[] = {
    {"_rmfr_mfrs_dag", (DL_FUNC) &_rmfr_mfrs_dag, 2},
    {NULL, NULL, 0}
};

RcppExport void R_init_rmfr(DllInfo *dll) {
    R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
    R_useDynamicSymbols(dll, FALSE);
}
