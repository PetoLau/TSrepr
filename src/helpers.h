#include <Rcpp.h>
using namespace Rcpp;

NumericVector movave(NumericVector x, int order);

double minC(NumericVector x);
double maxC(NumericVector x);
double meanC(NumericVector x);
double medianC(NumericVector x);
double sumC(NumericVector x);
