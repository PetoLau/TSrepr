#include <numeric>
#include <algorithm>
#include <Rcpp.h>
using namespace Rcpp;

//' @rdname norm_z
//' @name norm_z
//' @title Z-score normalisation
//'
//' @description The \code{norm_z} normalises time series by z-score.
//'
//' @return the numeric vector of normalised values
//'
//' @seealso \code{\link[TSrepr]{norm_min_max}}
//'
//' @param x the numeric vector (time series)
//'
//' @author Peter Laurinec, <tsreprpackage@gmail.com>
//'
//' @examples
//' norm_z(runif(50))
//'
//' @useDynLib TSrepr
//' @export norm_z
// [[Rcpp::export]]
NumericVector norm_z(NumericVector x) {

  int n = x.size();
  NumericVector x_norm(n);
  double sum = 0, mean = 0, sd = 0;

  for(int i = 0; i < n; ++i) {
    sum += x[i];
  }

  mean = sum / n;

  for(int i = 0; i < n; ++i) {
    sd += pow(x[i] - mean, 2);
  }

  sd = sqrt(sd/(n-1));

  if (sd == 0) {

    for(int i = 0; i < n; ++i){
      x_norm[i] = 0;
    }

  } else {

    for(int i = 0; i < n; ++i){
      x_norm[i] = (x[i] - mean) / sd;
    }

  }

  return x_norm;
}

//' @rdname norm_z_list
//' @name norm_z_list
//' @title Z-score normalization list
//'
//' @description The \code{norm_z_list} normalizes time series by z-score and returns normalization parameters (mean and standard deviation).
//'
//' @return the list composed of:
//'  \describe{
//'  \item{\strong{norm_values}}{the numeric vector of normalised values of time series}
//'  \item{\strong{mean}}{the mean value}
//'  \item{\strong{sd}}{the standard deviation}
//'   }
//'
//' @param x the numeric vector (time series)
//'
//' @seealso \code{\link[TSrepr]{norm_min_max_list}}
//'
//' @author Peter Laurinec, <tsreprpackage@gmail.com>
//'
//' @examples
//' norm_z_list(runif(50))
//'
//' @useDynLib TSrepr
//' @export norm_z_list
// [[Rcpp::export]]
List norm_z_list(NumericVector x) {

  int n = x.size();
  NumericVector x_norm(n);
  double sum = 0, mean = 0, sd = 0;

  for(int i = 0; i < n; ++i) {
    sum += x[i];
  }

  mean = sum / n;

  for(int i = 0; i < n; ++i) {
    sd += pow(x[i] - mean, 2);
  }

  sd = sqrt(sd/(n-1));

  if (sd == 0) {

    for(int i = 0; i < n; ++i){
      x_norm[i] = 0;
    }

  } else {

    for(int i = 0; i < n; ++i){
      x_norm[i] = (x[i] - mean) / sd;
    }

  }

  return List::create(
    _["norm_values"] = x_norm,
    _["mean"] = mean,
    _["sd"] = sd
  );
}

//' @rdname denorm_z
//' @name denorm_z
//' @title Z-score denormalisation
//'
//' @description The \code{denorm_z} denormalises time series by z-score method.
//'
//' @return the numeric vector of denormalised values
//'
//' @param x the numeric vector (time series)
//' @param mean the mean value
//' @param sd the standard deviation value
//'
//' @seealso \code{\link[TSrepr]{norm_z}, \link[TSrepr]{norm_z_list}}
//'
//' @author Peter Laurinec, <tsreprpackage@gmail.com>
//'
//' @references Laurinec P, Lucká M (2018)
//' Clustering-based forecasting method for individual consumers electricity load using time series representations.
//' Open Comput Sci, 8(1):38–50, DOI: 10.1515/comp-2018-0006
//'
//' @examples
//' # Normalise values and save normalisation parameters:
//' norm_res <- norm_z_list(rnorm(50, 5, 2))
//' # Denormalise new data with previous computed parameters:
//' denorm_z(rnorm(50, 4, 2), mean = norm_res$mean, sd = norm_res$sd)
//'
//' @useDynLib TSrepr
//' @export denorm_z
// [[Rcpp::export]]
NumericVector denorm_z(NumericVector x, double mean, double sd) {
  int n = x.size();
  NumericVector values(n);

  for(int i = 0; i < n; i++){
    values[i] = (x[i] * sd) + mean;
  }

  return values;
}

//' @rdname norm_z_params
//' @name norm_z_params
//' @title Z-score normalisation with parameters
//'
//' @description The \code{norm_z_params} normalises time series by z-score with defined mean and standard deviation.
//'
//' @return the numeric vector of normalised values
//'
//' @seealso \code{\link[TSrepr]{norm_min_max_params}}
//'
//' @param x the numeric vector (time series)
//' @param mean the numeric value
//' @param sd the numeric value - standard deviation
//'
//' @author Peter Laurinec, <tsreprpackage@gmail.com>
//'
//' @examples
//' norm_z_params(runif(50), 0.5, 1)
//'
//' @useDynLib TSrepr
//' @export norm_z_params
// [[Rcpp::export]]
NumericVector norm_z_params(NumericVector x, double mean, double sd) {

  int n = x.size();
  NumericVector x_norm(n);

  if (sd == 0) {

    for(int i = 0; i < n; ++i){
      x_norm[i] = 0;
    }

  } else {

    for(int i = 0; i < n; ++i){
      x_norm[i] = (x[i] - mean) / sd;
    }

  }

  return x_norm;

}

//' @rdname norm_min_max
//' @name norm_min_max
//' @title Min-Max normalisation
//'
//' @description The \code{norm_min_max} normalises time series by min-max method.
//'
//' @return the numeric vector of normalised values
//'
//' @param x the numeric vector (time series)
//'
//' @seealso \code{\link[TSrepr]{norm_z}}
//'
//' @author Peter Laurinec, <tsreprpackage@gmail.com>
//'
//' @examples
//' norm_min_max(rnorm(50))
//'
//' @useDynLib TSrepr
//' @export norm_min_max
// [[Rcpp::export]]
NumericVector norm_min_max(NumericVector x) {

  int n = x.size();
  NumericVector x_norm(n);
  double max_x = *std::max_element(x.begin(), x.end());
  double min_x = *std::min_element(x.begin(), x.end());

  if ((max_x - min_x) == 0) {

    for(int i = 0; i < n; ++i){
      x_norm[i] = 0;
    }

  } else {

    for(int i = 0; i < n; ++i){
      x_norm[i] = (x[i] - min_x) / (max_x - min_x);
    }

  }

  return x_norm;
}

//' @rdname norm_min_max_list
//' @name norm_min_max_list
//' @title Min-Max normalization list
//'
//' @description The \code{norm_min_max_list} normalises time series by min-max method and returns normalization parameters (min and max).
//'
//' @return the list composed of:
//'  \describe{
//'  \item{\strong{norm_values}}{the numeric vector of normalised values of time series}
//'  \item{\strong{min}}{the min value}
//'  \item{\strong{max}}{the max value}
//'   }
//'
//' @param x the numeric vector (time series)
//'
//' @seealso \code{\link[TSrepr]{norm_z_list}}
//'
//' @author Peter Laurinec, <tsreprpackage@gmail.com>
//'
//' @examples
//' norm_min_max_list(rnorm(50))
//'
//' @useDynLib TSrepr
//' @export norm_min_max_list
// [[Rcpp::export]]
List norm_min_max_list(NumericVector x) {

  int n = x.size();
  NumericVector x_norm(n);
  double max_x = *std::max_element(x.begin(), x.end());
  double min_x = *std::min_element(x.begin(), x.end());

  if ((max_x - min_x) == 0) {

    for(int i = 0; i < n; ++i){
      x_norm[i] = 0;
    }

  } else {

    for(int i = 0; i < n; ++i){
      x_norm[i] = (x[i] - min_x) / (max_x - min_x);
    }

  }

  return List::create(
    _["norm_values"] = x_norm,
    _["min"] = min_x,
    _["max"] = max_x
  );
}

//' @rdname denorm_min_max
//' @name denorm_min_max
//' @title Min-Max denormalisation
//'
//' @description The \code{denorm_min_max} denormalises time series by min-max method.
//'
//' @return the numeric vector of denormalised values
//'
//' @param x the numeric vector (time series)
//' @param min the minimum value
//' @param max the maximal value
//'
//' @seealso \code{\link[TSrepr]{norm_min_max}, \link[TSrepr]{norm_min_max_list}}
//'
//' @author Peter Laurinec, <tsreprpackage@gmail.com>
//'
//' @references Laurinec P, Lucká M (2018)
//' Clustering-based forecasting method for individual consumers electricity load using time series representations.
//' Open Comput Sci, 8(1):38–50, DOI: 10.1515/comp-2018-0006
//'
//' @examples
//' # Normalise values and save normalisation parameters:
//' norm_res <- norm_min_max_list(rnorm(50, 5, 2))
//' # Denormalise new data with previous computed parameters:
//' denorm_min_max(rnorm(50, 4, 2), min = norm_res$min, max = norm_res$max)
//'
//' @useDynLib TSrepr
//' @export denorm_min_max
// [[Rcpp::export]]
NumericVector denorm_min_max(NumericVector x, double min, double max) {
  int n = x.size();
  NumericVector values(n);

  for(int i = 0; i < n; i++){
    values[i] = (x[i] * (max - min)) + min;
  }

  return values;
}

//' @rdname norm_min_max_params
//' @name norm_min_max_params
//' @title Min-Max normalisation with parameters
//'
//' @description The \code{norm_min_max_params} normalises time series by min-max method with defined parameters.
//'
//' @return the numeric vector of normalised values
//'
//' @param x the numeric vector (time series)
//' @param min the numeric value
//' @param max the numeric value
//'
//' @seealso \code{\link[TSrepr]{norm_z_params}}
//'
//' @author Peter Laurinec, <tsreprpackage@gmail.com>
//'
//' @examples
//' norm_min_max_params(rnorm(50), 0, 1)
//'
//' @useDynLib TSrepr
//' @export norm_min_max_params
// [[Rcpp::export]]
NumericVector norm_min_max_params(NumericVector x, double min, double max) {

  int n = x.size();
  NumericVector x_norm(n);

  if ((max - min) == 0) {

    for(int i = 0; i < n; ++i){
      x_norm[i] = 0;
    }

  } else {

    for(int i = 0; i < n; ++i){
      x_norm[i] = (x[i] - min) / (max - min);
    }

  }

  return x_norm;

}
