// Generated by using Rcpp::compileAttributes() -> do not edit by hand
// Generator token: 10BE3573-1514-4C36-9D1C-5A225CD40393

#include <Rcpp.h>

using namespace Rcpp;

// marshall_bard
std::list<std::list<std::vector<double> > > marshall_bard(const std::list<std::vector<double> >& data, const double& p_N, const double& p_A, const double& k_N, const double& k_A, const double& pi_N, const double& alpha, const double& paffected, const double& lower, const double& upper, const double& h, const int& seed);
RcppExport SEXP _anomaly_marshall_bard(SEXP dataSEXP, SEXP p_NSEXP, SEXP p_ASEXP, SEXP k_NSEXP, SEXP k_ASEXP, SEXP pi_NSEXP, SEXP alphaSEXP, SEXP paffectedSEXP, SEXP lowerSEXP, SEXP upperSEXP, SEXP hSEXP, SEXP seedSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< const std::list<std::vector<double> >& >::type data(dataSEXP);
    Rcpp::traits::input_parameter< const double& >::type p_N(p_NSEXP);
    Rcpp::traits::input_parameter< const double& >::type p_A(p_ASEXP);
    Rcpp::traits::input_parameter< const double& >::type k_N(k_NSEXP);
    Rcpp::traits::input_parameter< const double& >::type k_A(k_ASEXP);
    Rcpp::traits::input_parameter< const double& >::type pi_N(pi_NSEXP);
    Rcpp::traits::input_parameter< const double& >::type alpha(alphaSEXP);
    Rcpp::traits::input_parameter< const double& >::type paffected(paffectedSEXP);
    Rcpp::traits::input_parameter< const double& >::type lower(lowerSEXP);
    Rcpp::traits::input_parameter< const double& >::type upper(upperSEXP);
    Rcpp::traits::input_parameter< const double& >::type h(hSEXP);
    Rcpp::traits::input_parameter< const int& >::type seed(seedSEXP);
    rcpp_result_gen = Rcpp::wrap(marshall_bard(data, p_N, p_A, k_N, k_A, pi_N, alpha, paffected, lower, upper, h, seed));
    return rcpp_result_gen;
END_RCPP
}
// marshall_MeanVarAnomaly
std::vector<int> marshall_MeanVarAnomaly(SEXP a, SEXP b, SEXP c, SEXP d, SEXP e, SEXP f, SEXP g);
RcppExport SEXP _anomaly_marshall_MeanVarAnomaly(SEXP aSEXP, SEXP bSEXP, SEXP cSEXP, SEXP dSEXP, SEXP eSEXP, SEXP fSEXP, SEXP gSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< SEXP >::type a(aSEXP);
    Rcpp::traits::input_parameter< SEXP >::type b(bSEXP);
    Rcpp::traits::input_parameter< SEXP >::type c(cSEXP);
    Rcpp::traits::input_parameter< SEXP >::type d(dSEXP);
    Rcpp::traits::input_parameter< SEXP >::type e(eSEXP);
    Rcpp::traits::input_parameter< SEXP >::type f(fSEXP);
    Rcpp::traits::input_parameter< SEXP >::type g(gSEXP);
    rcpp_result_gen = Rcpp::wrap(marshall_MeanVarAnomaly(a, b, c, d, e, f, g));
    return rcpp_result_gen;
END_RCPP
}
// marshall_MeanAnomaly
std::vector<int> marshall_MeanAnomaly(SEXP a, SEXP b, SEXP c, SEXP d, SEXP e, SEXP f, SEXP g);
RcppExport SEXP _anomaly_marshall_MeanAnomaly(SEXP aSEXP, SEXP bSEXP, SEXP cSEXP, SEXP dSEXP, SEXP eSEXP, SEXP fSEXP, SEXP gSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< SEXP >::type a(aSEXP);
    Rcpp::traits::input_parameter< SEXP >::type b(bSEXP);
    Rcpp::traits::input_parameter< SEXP >::type c(cSEXP);
    Rcpp::traits::input_parameter< SEXP >::type d(dSEXP);
    Rcpp::traits::input_parameter< SEXP >::type e(eSEXP);
    Rcpp::traits::input_parameter< SEXP >::type f(fSEXP);
    Rcpp::traits::input_parameter< SEXP >::type g(gSEXP);
    rcpp_result_gen = Rcpp::wrap(marshall_MeanAnomaly(a, b, c, d, e, f, g));
    return rcpp_result_gen;
END_RCPP
}
// marshall_recursive_anomalies
std::vector<int> marshall_recursive_anomalies(SEXP a, SEXP b, SEXP c);
RcppExport SEXP _anomaly_marshall_recursive_anomalies(SEXP aSEXP, SEXP bSEXP, SEXP cSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< SEXP >::type a(aSEXP);
    Rcpp::traits::input_parameter< SEXP >::type b(bSEXP);
    Rcpp::traits::input_parameter< SEXP >::type c(cSEXP);
    rcpp_result_gen = Rcpp::wrap(marshall_recursive_anomalies(a, b, c));
    return rcpp_result_gen;
END_RCPP
}
// marshall_recursive_mvanomalies
std::vector<int> marshall_recursive_mvanomalies(SEXP a, SEXP b, SEXP c, SEXP d, SEXP e, SEXP f, SEXP g);
RcppExport SEXP _anomaly_marshall_recursive_mvanomalies(SEXP aSEXP, SEXP bSEXP, SEXP cSEXP, SEXP dSEXP, SEXP eSEXP, SEXP fSEXP, SEXP gSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< SEXP >::type a(aSEXP);
    Rcpp::traits::input_parameter< SEXP >::type b(bSEXP);
    Rcpp::traits::input_parameter< SEXP >::type c(cSEXP);
    Rcpp::traits::input_parameter< SEXP >::type d(dSEXP);
    Rcpp::traits::input_parameter< SEXP >::type e(eSEXP);
    Rcpp::traits::input_parameter< SEXP >::type f(fSEXP);
    Rcpp::traits::input_parameter< SEXP >::type g(gSEXP);
    rcpp_result_gen = Rcpp::wrap(marshall_recursive_mvanomalies(a, b, c, d, e, f, g));
    return rcpp_result_gen;
END_RCPP
}
// marshall_MeanVarAnomalyMV
std::vector<int> marshall_MeanVarAnomalyMV(SEXP a, SEXP b, SEXP c, SEXP d, SEXP e, SEXP f, SEXP g, SEXP h, SEXP i);
RcppExport SEXP _anomaly_marshall_MeanVarAnomalyMV(SEXP aSEXP, SEXP bSEXP, SEXP cSEXP, SEXP dSEXP, SEXP eSEXP, SEXP fSEXP, SEXP gSEXP, SEXP hSEXP, SEXP iSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< SEXP >::type a(aSEXP);
    Rcpp::traits::input_parameter< SEXP >::type b(bSEXP);
    Rcpp::traits::input_parameter< SEXP >::type c(cSEXP);
    Rcpp::traits::input_parameter< SEXP >::type d(dSEXP);
    Rcpp::traits::input_parameter< SEXP >::type e(eSEXP);
    Rcpp::traits::input_parameter< SEXP >::type f(fSEXP);
    Rcpp::traits::input_parameter< SEXP >::type g(gSEXP);
    Rcpp::traits::input_parameter< SEXP >::type h(hSEXP);
    Rcpp::traits::input_parameter< SEXP >::type i(iSEXP);
    rcpp_result_gen = Rcpp::wrap(marshall_MeanVarAnomalyMV(a, b, c, d, e, f, g, h, i));
    return rcpp_result_gen;
END_RCPP
}
// marshall_MeanAnomalyMV
std::vector<int> marshall_MeanAnomalyMV(SEXP a, SEXP b, SEXP c, SEXP d, SEXP e, SEXP f, SEXP g, SEXP h, SEXP i);
RcppExport SEXP _anomaly_marshall_MeanAnomalyMV(SEXP aSEXP, SEXP bSEXP, SEXP cSEXP, SEXP dSEXP, SEXP eSEXP, SEXP fSEXP, SEXP gSEXP, SEXP hSEXP, SEXP iSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< SEXP >::type a(aSEXP);
    Rcpp::traits::input_parameter< SEXP >::type b(bSEXP);
    Rcpp::traits::input_parameter< SEXP >::type c(cSEXP);
    Rcpp::traits::input_parameter< SEXP >::type d(dSEXP);
    Rcpp::traits::input_parameter< SEXP >::type e(eSEXP);
    Rcpp::traits::input_parameter< SEXP >::type f(fSEXP);
    Rcpp::traits::input_parameter< SEXP >::type g(gSEXP);
    Rcpp::traits::input_parameter< SEXP >::type h(hSEXP);
    Rcpp::traits::input_parameter< SEXP >::type i(iSEXP);
    rcpp_result_gen = Rcpp::wrap(marshall_MeanAnomalyMV(a, b, c, d, e, f, g, h, i));
    return rcpp_result_gen;
END_RCPP
}
// marshall_sequential_ests
Rcpp::List marshall_sequential_ests(const std::vector<double>& data, int n, int burnin, double lqs, double lqf0, double meds, double medf0, double uqs, double uqf0);
RcppExport SEXP _anomaly_marshall_sequential_ests(SEXP dataSEXP, SEXP nSEXP, SEXP burninSEXP, SEXP lqsSEXP, SEXP lqf0SEXP, SEXP medsSEXP, SEXP medf0SEXP, SEXP uqsSEXP, SEXP uqf0SEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< const std::vector<double>& >::type data(dataSEXP);
    Rcpp::traits::input_parameter< int >::type n(nSEXP);
    Rcpp::traits::input_parameter< int >::type burnin(burninSEXP);
    Rcpp::traits::input_parameter< double >::type lqs(lqsSEXP);
    Rcpp::traits::input_parameter< double >::type lqf0(lqf0SEXP);
    Rcpp::traits::input_parameter< double >::type meds(medsSEXP);
    Rcpp::traits::input_parameter< double >::type medf0(medf0SEXP);
    Rcpp::traits::input_parameter< double >::type uqs(uqsSEXP);
    Rcpp::traits::input_parameter< double >::type uqf0(uqf0SEXP);
    rcpp_result_gen = Rcpp::wrap(marshall_sequential_ests(data, n, burnin, lqs, lqf0, meds, medf0, uqs, uqf0));
    return rcpp_result_gen;
END_RCPP
}
// marshall_pass
std::list<std::vector<double> > marshall_pass(const std::list<std::vector<double> >& Xi, const int& Lmax, const int& Lmin, const int& alpha, const double& lambda);
RcppExport SEXP _anomaly_marshall_pass(SEXP XiSEXP, SEXP LmaxSEXP, SEXP LminSEXP, SEXP alphaSEXP, SEXP lambdaSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< const std::list<std::vector<double> >& >::type Xi(XiSEXP);
    Rcpp::traits::input_parameter< const int& >::type Lmax(LmaxSEXP);
    Rcpp::traits::input_parameter< const int& >::type Lmin(LminSEXP);
    Rcpp::traits::input_parameter< const int& >::type alpha(alphaSEXP);
    Rcpp::traits::input_parameter< const double& >::type lambda(lambdaSEXP);
    rcpp_result_gen = Rcpp::wrap(marshall_pass(Xi, Lmax, Lmin, alpha, lambda));
    return rcpp_result_gen;
END_RCPP
}

static const R_CallMethodDef CallEntries[] = {
    {"_anomaly_marshall_bard", (DL_FUNC) &_anomaly_marshall_bard, 12},
    {"_anomaly_marshall_MeanVarAnomaly", (DL_FUNC) &_anomaly_marshall_MeanVarAnomaly, 7},
    {"_anomaly_marshall_MeanAnomaly", (DL_FUNC) &_anomaly_marshall_MeanAnomaly, 7},
    {"_anomaly_marshall_recursive_anomalies", (DL_FUNC) &_anomaly_marshall_recursive_anomalies, 3},
    {"_anomaly_marshall_recursive_mvanomalies", (DL_FUNC) &_anomaly_marshall_recursive_mvanomalies, 7},
    {"_anomaly_marshall_MeanVarAnomalyMV", (DL_FUNC) &_anomaly_marshall_MeanVarAnomalyMV, 9},
    {"_anomaly_marshall_MeanAnomalyMV", (DL_FUNC) &_anomaly_marshall_MeanAnomalyMV, 9},
    {"_anomaly_marshall_sequential_ests", (DL_FUNC) &_anomaly_marshall_sequential_ests, 9},
    {"_anomaly_marshall_pass", (DL_FUNC) &_anomaly_marshall_pass, 5},
    {NULL, NULL, 0}
};

RcppExport void R_init_anomaly(DllInfo *dll) {
    R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
    R_useDynamicSymbols(dll, FALSE);
}
