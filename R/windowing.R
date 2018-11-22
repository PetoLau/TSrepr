#' @rdname repr_windowing
#' @name repr_windowing
#' @title Windowing of time series
#'
#' @description The \code{repr_windowing} computes representations from windows of a vector.
#'
#' @return the numeric vector
#'
#' @param x the numeric vector (time series)
#' @param win_size the length of the window
#' @param func the function for representation computation. For example \code{repr_feaclip} or \code{repr_trend}.
#' @param args the list of additional arguments to the func (representation computation function). The args list must be named.
#'
#' @details This function applies specified representation method (function) to every non-overlapping window (subsequence, piece) of a time series.
#'
#' @author Peter Laurinec, <tsreprpackage@gmail.com>
#'
#' @references Laurinec P, and Lucka M (2018)
#' Interpretable multiple data streams clustering with clipped streams representation for the improvement of electricity consumption forecasting.
#' Data Mining and Knowledge Discovery. Springer. DOI: 10.1007/s10618-018-0598-2
#'
#' @seealso \code{\link[TSrepr]{repr_paa}, \link[TSrepr]{repr_matrix}}
#'
#' @examples
#' # func without arguments
#' repr_windowing(rnorm(48), win_size = 24, func = repr_feaclip)
#'
#' # func with arguments
#' repr_windowing(rnorm(48), win_size = 24, func = repr_featrend,
#'  args = list(func = maxC, order = 2, pieces = 2))
#'
#' @importFrom utils tail
#' @export repr_windowing
repr_windowing <- function(x, win_size, func = NULL, args = NULL) {

  if (is.null(func)) {
    stop("func must be specified!")
  }

  x <- as.numeric(x)

  n <- length(x)
  n_win <- floor(n / win_size)
  remain <- n %% win_size
  remain_count = n - (n_win * win_size)

  if (remain == 0) {
    repr <- c(sapply(0:(n_win - 1), function(i) do.call(func, args = append(list(x = x[((i*win_size)+1):((i+1)*win_size)]), args))))
  } else {

      repr <- c(sapply(0:(n_win - 1), function(i) do.call(func, args = append(list(x = x[((i*win_size)+1):((i+1)*win_size)]), args))))
      repr_rem <- do.call(func, args = append(list(x = tail(x, remain_count)), args))
      repr <- c(repr, repr_rem)
    }

  return(repr)
}
