#include <numeric>
#include <algorithm>
#include <Rcpp.h>
using namespace Rcpp;

//' @rdname rmse
//' @name rmse
//' @title RMSE
//'
//' @description \code{rmse} Computes RMSE of a forecast.
//'
//' @return numeric value
//'
//' @param x Numeric vector of real values
//' @param y Numeric vector of predicted values
//'
//' @examples
//' rmse(runif(50), runif(50))
//'
//' @export
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
//' @description \code{mae} Computes MAE of a forecast.
//'
//' @return numeric value
//'
//' @param x Numeric vector of real values
//' @param y Numeric vector of predicted values
//'
//' @examples
//' mae(runif(50), runif(50))
//'
//' @export
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
//' @description \code{smape} Computes sMAPE of a forecast.
//'
//' @return numeric value
//'
//' @param x Numeric vector of real values
//' @param y Numeric vector of predicted values
//'
//' @examples
//' smape(runif(50), runif(50))
//'
//' @export
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
//' @description \code{mape} Computes MAPE of a forecast.
//'
//' @return numeric value
//'
//' @param x Numeric vector of real values
//' @param y Numeric vector of predicted values
//'
//' @examples
//' mape(runif(50), runif(50))
//'
//' @export
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
//' @title MDAE
//'
//' @description \code{mdae} Computes MdAE of a forecast.
//'
//' @return numeric value
//'
//' @param x Numeric vector of real values
//' @param y Numeric vector of predicted values
//'
//' @examples
//' mdae(runif(50), runif(50))
//'
//' @export
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
//' @description \code{mase} Computes MASE of a forecast.
//' @return numeric value
//'
//' @param real Numeric vector of real values
//' @param forecast Numeric vector of predicted values
//' @param naive Numeric vector of naive forecast
//'
//' @examples
//' mase(rnorm(50), rnorm(50), rnorm(50))
//'
//' @export
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
