#include <numeric>
#include <algorithm>
#include <Rcpp.h>
using namespace Rcpp;

//' @rdname movave
//' @name movave
//' @title Simple Moving Average
//'
//' @description \code{movave} computes Simple Moving Average (SMA) from a time series.
//'
//' @return Numeric vector of smoothed values
//'
//' @param x Numeric vector
//' @param order - order of simple moving average
//'
//' @examples
//' movave(rnorm(50), 4)
//'
//' @export
// [[Rcpp::export]]
NumericVector movave(NumericVector x, int order) {

  int n = x.size();
  int n_ma = n - order;
  double sum = 0;

  NumericVector repr(n_ma);

  for(int i = 0; i < order; i++){
    sum += x[i];
  }

  repr[0] = sum / order;

  for(int i = 1; i < n_ma; i++){
    repr[i] = repr[i-1] + (x[i+order]/order) - (x[i-1]/order);
  }

  return repr;
}

//' @rdname fast_stat
//' @name fast_stat
//' @title Fast statistic functions (helpers)
//'
//' @description Fast statistic functions (helpers) for representations computation.
//'
//' @return Numeric value
//'
//' @param x Numeric vector
//'
//' @examples
//' maxC(rnorm(50))
//'
//' @export
// [[Rcpp::export]]
double maxC(NumericVector x) {
  double max;
  max = *std::max_element(x.begin(), x.end());

  return max;
}

//' @rdname fast_stat
//' @name fast_stat
//' @title Fast statistic functions (helpers)
//'
//' @examples
//' minC(rnorm(50))
//'
//' @export
// [[Rcpp::export]]
double minC(NumericVector x) {
  double min;
  min = *std::min_element(x.begin(), x.end());

  return min;
}

//' @rdname fast_stat
//' @name fast_stat
//' @title Fast statistic functions (helpers)
//'
//' @examples
//' meanC(rnorm(50))
//'
//' @export
// [[Rcpp::export]]
double meanC(NumericVector x) {
  int n = x.size();
  double total = 0;

  for(int i = 0; i < n; ++i) {
    total += x[i];
  }
  return total / n;
}

//' @rdname fast_stat
//' @name fast_stat
//' @title Fast statistic functions (helpers)
//'
//' @examples
//' sumC(rnorm(50))
//'
//' @export
// [[Rcpp::export]]
double sumC(NumericVector x) {
  int n = x.size();
  double total = 0;
  for(int i = 0; i < n; ++i) {
    total += x[i];
  }
  return total;
}

//' @rdname fast_stat
//' @name fast_stat
//' @title Fast statistic functions (helpers)
//'
//' @examples
//' medianC(rnorm(50))
//'
//' @export
// [[Rcpp::export]]
double medianC(NumericVector x) {
  NumericVector y = clone(x);
  int n, half;
  double y1, y2;
  n = y.size();
  half = n / 2;
  if(n % 2 == 1) {
    // median for odd length vector
    std::nth_element(y.begin(), y.begin()+half, y.end());
    return y[half];
  } else {
    // median for even length vector
    std::nth_element(y.begin(), y.begin()+half, y.end());
    y1 = y[half];
    std::nth_element(y.begin(), y.begin()+half-1, y.begin()+half);
    y2 = y[half-1];
    return (y1 + y2) / 2.0;
  }
}
