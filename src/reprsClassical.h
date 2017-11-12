#include <Rcpp.h>
using namespace Rcpp;

NumericVector repr_sma(NumericVector x, int order);
NumericVector repr_paa(NumericVector x, int q, Rcpp::Function func);
NumericVector repr_seas_profile(NumericVector x, int freq, Rcpp::Function func);
