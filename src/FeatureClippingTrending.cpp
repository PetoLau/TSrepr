#include <numeric>
#include <algorithm>
#include <Rcpp.h>
#include "helpers.h"
#include "rle.h"
#include "reprsClassical.h"
using namespace Rcpp;

//' @rdname clipping
//' @name clipping
//' @title Creates bit-level (clipped representation) from a vector
//'
//' @description The \code{clipping} computes bit-level (clipped representation) from a vector.
//'
//' @return the integer vector of zeros and ones
//'
//' @param x the numeric vector (time series)
//'
//' @details Clipping transforms time series to bit-level representation.
//'
//' It is defined as follows:
//' \deqn{repr_t   =   {1   if   x_t   >   \mu ,  0  otherwise,}}{repr_t  =   {1   if   x_t   >   \mu ,  0   otherwise,}} where \eqn{x_t} is a value of a time series
//' and \eqn{\mu} is average of a time series.
//'
//' @seealso \code{\link[TSrepr]{trending}}
//'
//' @author Peter Laurinec, <tsreprpackage@gmail.com>
//'
//' @references Bagnall A, Ratanamahatana C, Keogh E, Lonardi S, Janacek G (2006)
//' A bit level representation for time series data mining with shape based similarity.
//' Data Mining and Knowledge Discovery 13(1):11-40
//'
//' Laurinec P, and Lucka M (2018)
//' Interpretable multiple data streams clustering with clipped streams representation for the improvement of electricity consumption forecasting.
//' Data Mining and Knowledge Discovery. Springer. DOI: 10.1007/s10618-018-0598-2
//'
//' @importFrom Rcpp evalCpp
//'
//' @examples
//' clipping(rnorm(50))
//'
//' @useDynLib TSrepr
//' @export clipping
// [[Rcpp::export]]
IntegerVector clipping(NumericVector x) {
  int n = x.size();
  IntegerVector bitLevel(n);
  double x_mean = 0;

  x_mean = std::accumulate(x.begin(), x.end(), 0.0) / n;

  for(int i = 0; i < n; ++i) {
    if(x[i] > x_mean) {
      bitLevel[i] = 1;
    } else bitLevel[i] = 0;
  }

  return bitLevel;
}

//' @rdname trending
//' @name trending
//' @title Creates bit-level (trending) representation from a vector
//'
//' @description The \code{trending} Computes bit-level (trending) representation from a vector.
//'
//' @return the integer vector of zeros and ones
//'
//' @param x the numeric vector (time series)
//'
//' @details Trending transforms time series to bit-level representation.
//'
//' It is defined as follows:
//' \deqn{repr_t   =   {1   if   x_t  -  x_{t+1}  <  0 ,  0   otherwise,}}{repr_t   =   {1   if   x_t  -  x_{t+1}  <  0 ,  0   otherwise,}}
//' where \eqn{x_t} is a value of a time series.
//'
//' @seealso \code{\link[TSrepr]{clipping}}
//'
//' @author Peter Laurinec, <tsreprpackage@gmail.com>
//'
//' @examples
//' trending(rnorm(50))
//'
//' @useDynLib TSrepr
//' @export trending
// [[Rcpp::export]]
IntegerVector trending(NumericVector x) {

  int n = x.size();
  IntegerVector repr(n-1);

  for(int i = 0; i < n-1; i++){
    if((x[i] - x[i+1]) < 0) {
      repr[i] = 1;
    } else repr[i] = 0;
  }

  return repr;
}

//' @rdname repr_feaclip
//' @name repr_feaclip
//' @title FeaClip representation of time series
//'
//' @description The \code{repr_feaclip} computes representation of time series based on feature extraction from bit-level (clipped) representation.
//'
//' @return the numeric vector of length 8
//'
//' @param x the numeric vector (time series)
//'
//' @details FeaClip is method of time series representation based on feature extraction from run lengths (RLE) of bit-level (clipped) representation.
//' It extracts 8 key features from clipped representation.
//'
//' There are as follows: \deqn{repr   =  \{  max_1  -  max.  from  run  lengths  of  ones,}
//' \deqn{sum_1 -  sum  of  run  lengths  of  ones,}
//' \deqn{max_0  -  max.  from  run  lengths  of  zeros,}
//' \deqn{crossings  -  length  of  RLE  encoding  -  1,}
//' \deqn{f_0  -  number  of   first  zeros,}
//' \deqn{l_0  -  number  of  last  zeros,}
//' \deqn{f_1  -  number  of  first  ones,}
//' \deqn{l_1  -  number  of  last  ones  \}  .}
//'
//' @seealso \code{\link[TSrepr]{repr_featrend}, \link[TSrepr]{repr_feacliptrend}}
//'
//' @author Peter Laurinec, <tsreprpackage@gmail.com>
//'
//' @references Laurinec P, and Lucka M (2018)
//' Interpretable multiple data streams clustering with clipped streams representation for the improvement of electricity consumption forecasting.
//' Data Mining and Knowledge Discovery. Springer. DOI: 10.1007/s10618-018-0598-2
//'
//' @examples
//' repr_feaclip(rnorm(50))
//'
//' @useDynLib TSrepr
//' @export repr_feaclip
// [[Rcpp::export]]
NumericVector repr_feaclip(NumericVector x) {

  NumericVector y;
  Rcpp::List encode;
  NumericVector representation(8);
  int N, j = 0, k = 0;

  y = clipping(x);

  encode = rleC(y);

  IntegerVector lengths = encode["lengths"];
  IntegerVector values = encode["values"];

  N = values.size();

  representation[3] = N - 1;

  if(values[0] == 0) {
    representation[4] = lengths[0];
  } else representation[4] = 0;

  if(values[0] == 1) {
    representation[6] = lengths[0];
  } else representation[6] = 0;

  if(values[N-1] == 0) {
    representation[5] = lengths[N-1];
  } else representation[5] = 0;

  if(values[N-1] == 1) {
    representation[7] = lengths[N-1];
  } else representation[7] = 0;

  for(int i = 0; i < N; ++i) {
    if(values[i] == 0) {
      j = j + 1;
    } else {
      k = k + 1;
    }
  }

  std::vector<int> zeros(j), ones(k);

  j = 0;
  k = 0;

  for(int i = 0; i < N; ++i) {
    if(values[i] == 0) {
      zeros[j] = lengths[i];
      j = j + 1;
    } else {
      ones[k] = lengths[i];
      k = k + 1;
    }
  }

  if(ones.size() == 0) {
    representation[0] = 0;
    representation[1] = 0;
  } else {
    representation[0] = *std::max_element(ones.begin(), ones.end());
    representation[1] = std::accumulate(ones.begin(), ones.end(), 0.0);
  }

  if(zeros.size() == 0) {
    representation[2] = 0;
  } else {
    representation[2] = *std::max_element(zeros.begin(), zeros.end());
  }

  StringVector fea_name = StringVector::create("max_1", "sum_1", "max_0", "cross.", "f_0", "l_0", "f_1", "l_1");
  representation.attr("names") = fea_name;
  return representation;
}

//' @rdname repr_featrend
//' @name repr_featrend
//' @title FeaTrend representation of time series
//'
//' @description The \code{repr_featrend} computes representation of time series based on feature extraction from bit-level (trending) representation.
//'
//' @return the numeric vector of the length pieces
//'
//' @param x the numeric vector (time series)
//' @param func the function of aggregation, can be sumC or maxC or similar aggregation function
//' @param pieces the number of parts of time series to split (default to 2)
//' @param order the order of simple moving average (default to 4)
//'
//' @details FeaTrend is method of time series representation based on feature extraction from run lengths (RLE) of bit-level (trending) representation.
//' It extracts number of features from trending representation based on number of pieces defined.
//' From every piece, 2 features are extracted. You can define what feature will be extracted,
//' recommended functions are max and sum. For example if max is selected, then maximum value of run lengths of ones and zeros are extracted.
//'
//' @seealso \code{\link[TSrepr]{repr_feaclip}, \link[TSrepr]{repr_feacliptrend}}
//'
//' @author Peter Laurinec, <tsreprpackage@gmail.com>
//'
//' @examples
//' # default settings
//' repr_featrend(rnorm(50), maxC)
//'
//' # compute FeaTrend for 4 pieces and make more smoothed ts by order = 8
//' repr_featrend(rnorm(50), sumC, 4, 8)
//'
//' @useDynLib TSrepr
//' @export repr_featrend
// [[Rcpp::export]]
NumericVector repr_featrend(NumericVector x, Rcpp::Function func, int pieces = 2, int order = 4) {

  NumericVector sma_x;

  sma_x = repr_sma(x, order);

  NumericVector y;
  Rcpp::List encode;
  NumericVector repr(pieces*2);
  int n = sma_x.size(), N;
  int n_piece = n / pieces;
  IntegerVector x_ind(n_piece);
  int o = 0, z = 0;

  for(int j = 0; j < pieces; j++){
    for(int i = 0; i < n_piece; i++){
      x_ind[i] = i + (n_piece*j);
    }

    y = trending(sma_x[x_ind]);

    encode = rleC(y);

    IntegerVector lengths = encode["lengths"];
    IntegerVector values = encode["values"];

    N = values.size();

    for(int i = 0; i < N; ++i) {
      if(values[i] == 0) {
        z = z + 1;
      } else {
        o = o + 1;
      }
    }

    std::vector<int> zeros(z), ones(o);

    z = 0;
    o = 0;

    for(int i = 0; i < N; ++i) {
      if(values[i] == 0) {
        zeros[z] = lengths[i];
        z = z + 1;
      } else {
        ones[o] = lengths[i];
        o = o + 1;
      }
    }

    if(ones.size() == 0) {
      repr[j*2] = 0;
    } else {
       repr[j*2] = Rcpp::as<double>(func(ones));
    }

    if(zeros.size() == 0) {
      repr[j*2 + 1] = 0;
    } else {
       repr[j*2 +1] = Rcpp::as<double>(func(zeros));
    }
  }

  return repr;
}

//' @rdname repr_feacliptrend
//' @name repr_feacliptrend
//' @title FeaClipTrend representation of time series
//'
//' @description The \code{repr_feacliptrend} computes representation of time series
//' based on feature extraction from bit-level representations (clipping and trending).
//'
//' @return the numeric vector of frequencies of features
//'
//' @param x the numeric vector (time series)
//' @param func the aggregation function for FeaTrend procedure (sumC or maxC)
//' @param pieces the number of parts of time series to split
//' @param order the order of simple moving average
//'
//' @details FeaClipTrend combines FeaClip and FeaTrend representation methods.
//' See documentation of these two methods (check See Also section).
//'
//' @seealso \code{\link[TSrepr]{repr_featrend}, \link[TSrepr]{repr_feaclip}}
//'
//' @author Peter Laurinec, <tsreprpackage@gmail.com>
//'
//' @references Laurinec P, and Lucka M (2018)
//' Interpretable multiple data streams clustering with clipped streams representation for the improvement of electricity consumption forecasting.
//' Data Mining and Knowledge Discovery. Springer. DOI: 10.1007/s10618-018-0598-2
//'
//' @examples
//' repr_feacliptrend(rnorm(50), maxC, 2, 4)
//'
//' @useDynLib TSrepr
//' @export repr_feacliptrend
// [[Rcpp::export]]
std::vector<double> repr_feacliptrend(NumericVector x, Rcpp::Function func, int pieces = 2, int order = 4) {

  std::vector<double> repr;
  NumericVector repr_clip(8), repr_trend(pieces * 2);
  repr_clip = repr_feaclip(x);
  repr_trend = repr_featrend(x, func, pieces, order);

  repr.reserve( repr_clip.size() + repr_trend.size() );

  repr.insert( repr.end(), repr_clip.begin(), repr_clip.end() );
  repr.insert( repr.end(), repr_trend.begin(), repr_trend.end() );

  return repr;
}
