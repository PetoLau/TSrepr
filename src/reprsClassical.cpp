#include <numeric>
#include <algorithm>
#include <Rcpp.h>
#include "helpers.h"
using namespace Rcpp;

//' @rdname repr_sma
//' @name repr_sma
//' @title Simple Moving Average representation
//'
//' @description The \code{repr_sma} computes Simple Moving Average (SMA) from a time series.
//'
//' @return the numeric vector of smoothed values of the length = length(x) - order + 1
//'
//' @param x the numeric vector (time series)
//' @param order the order of simple moving average
//'
//' @author Peter Laurinec, <tsreprpackage@gmail.com>
//'
//' @examples
//' repr_sma(rnorm(50), 4)
//'
//' @useDynLib TSrepr
//' @export repr_sma
// [[Rcpp::export]]
NumericVector repr_sma(NumericVector x, int order) {

  int n = x.size();

  if (order <= 0) {

    stop("order must be positive number!");

  } else if (order > n) {

    stop("order must be less than length(x)!");

  }

  int n_ma = n - order + 1;
  double sum = 0;
  NumericVector repr(n_ma);

  for(int i = 0; i < order; i++){
    sum += x[i];
  }

  repr[0] = sum / order;

  for(int i = 1; i < n_ma; i++){
    repr[i] = repr[i-1] + (x[i+order-1]/order) - (x[i-1]/order);
  }

  return repr;

}

//' @rdname repr_paa
//' @name repr_paa
//' @title PAA - Piecewise Aggregate Approximation
//'
//' @description The \code{repr_paa} computes PAA representation from a vector.
//'
//' @return the numeric vector
//'
//' @param x the numeric vector (time series)
//' @param q the integer of the length of the "piece"
//' @param func the aggregation function. Can be meanC, medianC, sumC, minC or maxC or similar aggregation function
//'
//' @details PAA with possibility to use arbitrary aggregation function.
//' The original method uses average as aggregation function.
//'
//' @seealso \code{\link[TSrepr]{repr_dwt}, \link[TSrepr]{repr_dft}, \link[TSrepr]{repr_dct}, \link[TSrepr]{repr_sma}}
//'
//' @author Peter Laurinec, <tsreprpackage@gmail.com>
//'
//' @references Keogh E, Chakrabarti K, Pazzani M, Mehrotra Sh (2001)
//' Dimensionality Reduction for Fast Similarity Search in Large Time Series Databases.
//' Knowledge and Information Systems 3(3):263-286
//'
//' @examples
//' repr_paa(rnorm(11), 2, meanC)
//'
//' @useDynLib TSrepr
//' @export repr_paa
// [[Rcpp::export]]
NumericVector repr_paa(NumericVector x, int q, Rcpp::Function func) {

  int n = x.size();
  int n_paa = n/q;
  int remain = n % q;
  int remain_count = n - (n_paa*q);
  if (remain != 0) {
    n_paa = n_paa + 1;
  }

  NumericVector repr(n_paa);
  IntegerVector sub_x(q);
  IntegerVector sub_rem(remain_count);

  if (remain == 0) {

    for(int i = 0; i < n_paa; i++){
      for(int j = 0; j < q; j++){
        sub_x[j] = (i*q) + j;
      }
      repr[i] = Rcpp::as<double>(func(x[sub_x]));
    }

  } else {

    for(int i = 0; i < n_paa-1; i++){
      for(int j = 0; j < q; j++){
        sub_x[j] = (i*q) + j;
      }
      repr[i] = Rcpp::as<double>(func(x[sub_x]));
    }

    for(int j = 0; j < remain_count; j++){
      sub_rem[j] = ((n_paa-1)*q) + j;
    }
    repr[n_paa-1] = Rcpp::as<double>(func(x[sub_rem]));

  }

  return repr;
}

//' @rdname repr_seas_profile
//' @name repr_seas_profile
//' @title Mean seasonal profile of time series
//'
//' @description The \code{repr_seas_profile} computes mean seasonal profile representation from a time series.
//'
//' @return the numeric vector
//'
//' @param x the numeric vector (time series)
//' @param freq the integer of the length of the season
//' @param func the aggregation function. Can be meanC or medianC or similar aggregation function.
//'
//' @details This function computes mean seasonal profile representation for a seasonal time series.
//' The length of representation is length of set seasonality (frequency) of a time series.
//' Aggregation function is arbitrary (best choice is for you maybe mean or median).
//'
//' @author Peter Laurinec, <tsreprpackage@gmail.com>
//'
//' @references Laurinec P, Lucka M (2016)
//' Comparison of representations of time series for clustering smart meter data.
//' In: Lecture Notes in Engineering and Computer Science: Proceedings of The World Congress on Engineering and Computer Science 2016, pp 458-463
//'
//' Laurinec P, Loderer M, Vrablecova P, Lucka M, Rozinajova V, Ezzeddine AB (2016)
//' Adaptive time series forecasting of energy consumption using optimized cluster analysis.
//' In: Data Mining Workshops (ICDMW), 2016 IEEE 16th International Conference on, IEEE, pp 398-405
//'
//' Laurinec P, Lucká M (2018)
//' Clustering-based forecasting method for individual consumers electricity load using time series representations.
//' Open Comput Sci, 8(1):38–50, DOI: 10.1515/comp-2018-0006
//'
//' @seealso \code{\link[TSrepr]{repr_lm}, \link[TSrepr]{repr_gam}, \link[TSrepr]{repr_exp}}
//'
//' @examples
//' repr_seas_profile(rnorm(48*10), 48, meanC)
//'
//' @useDynLib TSrepr
//' @export repr_seas_profile
// [[Rcpp::export]]
NumericVector repr_seas_profile(NumericVector x, int freq, Rcpp::Function func) {

  NumericVector repr(freq);
  int n = x.size();
  // double freq_times = n / freq;
  int freq_times_int = n / freq;
  int remainder = n - (freq_times_int * freq);
  int n_times;
  IntegerVector ind;

  if (remainder == 0) {
    ind = static_cast<IntegerVector>(no_init(freq_times_int));

    for(int i = 0; i < freq; i++){
      for(int j = 0; j < freq_times_int; j++){
        ind[j] = (j*freq) + i;
      }
      repr[i] = Rcpp::as<double>(func(x[ind]));
    }
  } else {
    for(int i = 0; i < freq; i++){
      if ((i + 1) <= remainder) {
        n_times = freq_times_int + 1;
        ind = static_cast<IntegerVector>(no_init(n_times));
      } else {
        n_times = freq_times_int;
        ind = static_cast<IntegerVector>(no_init(n_times));
      }
      for(int j = 0; j < n_times; j++){
        ind[j] = (j*freq) + i;
      }
      repr[i] = Rcpp::as<double>(func(x[ind]));
    }
  }

  return repr;
}
