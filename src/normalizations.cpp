#include <numeric>
#include <algorithm>
#include <Rcpp.h>
using namespace Rcpp;

//' @rdname norm_z
//' @name norm_z
//' @title Z normalization
//'
//' @description \code{norm_z} normalizes data by z-score.
//'
//' @return numeric vector of normalized values
//'
//' @seealso \code{\link[TSrepr]{norm_min_max}}
//'
//' @param x Numeric vector
//'
//' @examples
//' norm_z(runif(50))
//'
//' @export
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
//' @title Z normalization list
//'
//' @description \code{norm_z_list} normalizes data by z-score and returns normalization parameters.
//'
//' @return list of numeric vector of normalized values and mean-sd values
//'
//' @param x Numeric vector
//'
//' @seealso \code{\link[TSrepr]{norm_min_max_list}}
//'
//' @examples
//' norm_z_list(runif(50))
//'
//' @export
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

//' @rdname norm_min_max
//' @name norm_min_max
//' @title min-max normalization
//'
//' @description \code{norm_min_max} normalizes data by min-max method.
//'
//' @return numeric vector of normalized values
//'
//' @param x Numeric vector
//'
//' @seealso \code{\link[TSrepr]{norm_z}}
//'
//' @examples
//' norm_min_max(rnorm(50))
//'
//' @export
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
//' @title min-max normalization list
//'
//' @description \code{norm_min_max_list} normalizes data by min-max method and returns normalization parameters.
//'
//' @return list of numeric vector of normalized values and min-max values
//'
//' @param x Numeric vector
//'
//' @seealso \code{\link[TSrepr]{norm_z_list}}
//'
//' @examples
//' norm_min_max_list(rnorm(50))
//'
//' @export
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
