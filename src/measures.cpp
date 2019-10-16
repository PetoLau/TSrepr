#include <numeric>
#include <algorithm>
#include <Rcpp.h>
using namespace Rcpp;

//' @rdname mse
//' @name mse
//' @title MSE
//'
//' @description The \code{mse} computes MSE (Mean Squared Error) of a forecast.
//'
//' @return the numeric value
//'
//' @param x the numeric vector of real values
//' @param y the numeric vector of forecasted values
//'
//' @author Peter Laurinec, <tsreprpackage@gmail.com>
//'
//' @examples
//' mse(runif(50), runif(50))
//'
//' @useDynLib TSrepr
//' @export mse
// [[Rcpp::export]]
double mse(NumericVector x, NumericVector y) {

  if (x.size() != y.size()) {
    stop("x and y have not the same length!");
  }

  int n = x.size();
  double total = 0;

  for(int i = 0; i < n; ++i) {
    total += pow(x[i]-y[i], 2.0);
  }
  return total / n;
}

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

  if (x.size() != y.size()) {
    stop("x and y have not the same length!");
  }

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

  if (x.size() != y.size()) {
    stop("x and y have not the same length!");
  }

  int n = x.size();
  double total = 0;

  for(int i = 0; i < n; ++i) {
    total += std::abs(x[i]-y[i]);
  }
  return total / n;
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

  if (x.size() != y.size()) {
    stop("x and y have not the same length!");
  }

  int size = x.size();
  NumericVector diff = abs(x - y);
  std::sort(&diff[0], &diff[size]);
  double median = size % 2 ? diff[size / 2] : (diff[size / 2 - 1] + diff[size / 2]) / 2;

  return median;
}

//' @rdname smape
//' @name smape
//' @title sMAPE
//'
//' @description The \code{smape} computes sMAPE (Symmetric Mean Absolute Percentage Error) of a forecast.
//'
//' @return the numeric value in \%
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

  if (x.size() != y.size()) {
    stop("x and y have not the same length!");
  }

  int n = x.size();
  double total = 0;

  for(int i = 0; i < n; ++i) {

    if (x[i] == 0 && y[i] == 0) {

      total += 0;

    } else {

      total += std::abs(x[i] - y[i]) / ((std::abs(x[i]) + std::abs(y[i])) / 2);

    }

  }

  return 100 * (total / n);
}

//' @rdname mape
//' @name mape
//' @title MAPE
//'
//' @description the \code{mape} computes MAPE (Mean Absolute Percentage Error) of a forecast.
//'
//' @return the numeric value in \%
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

  if (x.size() != y.size()) {
    stop("x and y have not the same length!");
  }

  int n = x.size();
  double total = 0;

  for(int i = 0; i < n; ++i) {

    if (x[i] == 0 && y[i] == 0) {

      total += 0;

    } else {

      total += std::abs((x[i] - y[i]) / x[i]);

    }

  }

  return 100 * (total / n);
}

//' @rdname maape
//' @name maape
//' @title MAAPE
//'
//' @description the \code{maape} computes MAAPE (Mean Arctangent Absolute Percentage Error) of a forecast.
//'
//' @return the numeric value in \%
//'
//' @param x the numeric vector of real values
//' @param y the numeric vector of forecasted values
//'
//' @author Peter Laurinec, <tsreprpackage@gmail.com>
//'
//' @references Sungil Kim, Heeyoung Kim (2016)
//' A new metric of absolute percentage error for intermittent demand forecasts,
//'  International Journal of Forecasting 32(3):669-679
//'
//' @examples
//' maape(runif(50), runif(50))
//'
//' @useDynLib TSrepr
//' @export maape
// [[Rcpp::export]]
double maape(NumericVector x, NumericVector y) {

  if (x.size() != y.size()) {
    stop("x and y have not the same length!");
  }

  int n = x.size();
  double total = 0;

  for(int i = 0; i < n; ++i) {

    if (x[i] == 0 && y[i] == 0) {

      total += 0;

    } else {

      total += atan(std::abs((x[i] - y[i]) / x[i]));

    }

  }

  return 100 * (total / n);
}

//' @rdname mase
//' @name mase
//' @title MASE
//'
//' @description The \code{mase} computes MASE (Mean Absolute Scaled Error) of a forecast.
//'
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

  if (real.size() != forecast.size() || real.size() != naive.size()) {
    stop("real, forecast and naive have not the same length!");
  }

  int n = real.size();
  double diff = 0, denom = 0, error = 0;

  for(int i = 0; i < n; ++i) {
    diff += std::abs((real[i] - forecast[i]));
    denom += std::abs((real[i] - naive[i]));
  }

  if (denom == 0) {
    error = 1;
  } else error = diff / denom;

  return error;
}
