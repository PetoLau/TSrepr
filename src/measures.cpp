#include <numeric>
#include <algorithm>
#include <Rcpp.h>
using namespace Rcpp;

//' @rdname rmse
//' @name rmse
//' @title RMSE
//'
//' @description The \code{rmse} computes RMSE (Root Mean Squared Error) of a forecast.
//'
//' @return the numeric value
//'
//' @param x the numeric vector of real values
//' @param y the numeric vector of forecasted values
//'
//' @author Peter Laurinec, <tsreprpackage@gmail.com>
//'
//' @examples
//' rmse(runif(50), runif(50))
//'
//' @useDynLib TSrepr
//' @export rmse
// [[Rcpp::export]]
double rmse(NumericVector x, NumericVector y) {
  int n = x.size();
  double total = 0;

  for(int i = 0; i < n; ++i) {
    total += pow(x[i]-y[i], 2.0);
  }
  return sqrt(total / n);
}

//' @rdname mae
//' @name mae
//' @title MAE
//'
//' @description The \code{mae} computes MAE (Mean Absolute Error) of a forecast.
//'
//' @return the numeric value
//'
//' @param x the numeric vector of real values
//' @param y the numeric vector of forecasted values
//'
//' @author Peter Laurinec, <tsreprpackage@gmail.com>
//'
//' @examples
//' mae(runif(50), runif(50))
//'
//' @useDynLib TSrepr
//' @export mae
// [[Rcpp::export]]
double mae(NumericVector x, NumericVector y) {
  int n = x.size();
  double total = 0;

  for(int i = 0; i < n; ++i) {
    total += std::abs(x[i]-y[i]);
  }
  return total / n;
}

//' @rdname smape
//' @name smape
//' @title sMAPE
//'
//' @description The \code{smape} computes sMAPE (Symmetric Mean Absolute Percentage Error) of a forecast.
//'
//' @return the numeric value
//'
//' @param x the numeric vector of real values
//' @param y the numeric vector of forecasted values
//'
//' @author Peter Laurinec, <tsreprpackage@gmail.com>
//'
//' @examples
//' smape(runif(50), runif(50))
//'
//' @useDynLib TSrepr
//' @export smape
// [[Rcpp::export]]
double smape(NumericVector x, NumericVector y) {
  int n = x.size();
  double total = 0;

  for(int i = 0; i < n; ++i) {
    total += std::abs(x[i] - y[i]) / ((std::abs(x[i]) + std::abs(y[i])) / 2);
  }

  return 100 * (total / n);
}

//' @rdname mape
//' @name mape
//' @title MAPE
//'
//' @description the \code{mape} computes MAPE (Mean Absolute Percentage Error) of a forecast.
//'
//' @return the numeric value
//'
//' @param x the numeric vector of real values
//' @param y the numeric vector of forecasted values
//'
//' @author Peter Laurinec, <tsreprpackage@gmail.com>
//'
//' @examples
//' mape(runif(50), runif(50))
//'
//' @useDynLib TSrepr
//' @export mape
// [[Rcpp::export]]
double mape(NumericVector x, NumericVector y) {
  int n = x.size();
  double total = 0;

  for(int i = 0; i < n; ++i) {
    total += std::abs((x[i] - y[i]) / x[i]);
  }

  return 100 * (total / n);
}

//' @rdname mdae
//' @name mdae
//' @title MdAE
//'
//' @description The \code{mdae} computes MdAE (Median Absolute Error) of a forecast.
//'
//' @return the numeric value
//'
//' @param x the numeric vector of real values
//' @param y the numeric vector of forecasted values
//'
//' @author Peter Laurinec, <tsreprpackage@gmail.com>
//'
//' @examples
//' mdae(runif(50), runif(50))
//'
//' @useDynLib TSrepr
//' @export mdae
// [[Rcpp::export]]
double mdae(NumericVector x, NumericVector y) {
  int size = x.size();
  NumericVector diff = abs(x - y);
  std::sort(&diff[0], &diff[size]);
  double median = size % 2 ? diff[size / 2] : (diff[size / 2 - 1] + diff[size / 2]) / 2;

  return median;
}

//' @rdname mase
//' @name mase
//' @title MASE
//'
//' @description The \code{mase} computes MASE (Mean Absolute Scaled Error) of a forecast.
//' @return the numeric value
//'
//' @param real the numeric vector of real values
//' @param forecast the numeric vector of forecasted values
//' @param naive the numeric vector of naive forecast
//'
//' @author Peter Laurinec, <tsreprpackage@gmail.com>
//'
//' @examples
//' mase(rnorm(50), rnorm(50), rnorm(50))
//'
//' @useDynLib TSrepr
//' @export mase
// [[Rcpp::export]]
double mase(NumericVector real, NumericVector forecast, NumericVector naive) {
  int n = real.size();
  double diff= 0, denom = 0, error = 0;

  for(int i = 0; i < n; ++i) {
    diff += std::abs((real[i] - forecast[i]));
    denom += std::abs((real[i] - naive[i]));
  }

  if (denom == 0){
    error = 1;
  } else error = diff / denom;

  return error;
}
