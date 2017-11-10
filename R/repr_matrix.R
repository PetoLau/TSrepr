# Computation of matrix of representations from matrix of time series

#' @rdname repr_matrix
#' @name repr_matrix
#' @title Computation of matrix of representations from matrix of time series
#'
#' @description The \code{repr_matrix} computes matrix of representations from matrix of time series
#'
#' @return the numeric matrix of representations of time series
#'
#' @param x the matrix, data.frame or data.table of time series, where time series are in rows of the table
#' @param func the function that computes representation
#' @param args the list of additional (or required) parameters of func (function that computes representation)
#' @param normalize normalise (scale) representations? (default is TRUE)
#' @param func_norm the normalization function (default is \code{norm_z})
#'
#' @details TODO.
#'
#' @examples
#' # Create random matrix of time series
#' mat_ts <- matrix(rnorm(100), ncol = 10)
#' repr_matrix(mat_ts, func = repr_paa, args = list(q = 5, func = meanC))
#'
#' @export repr_matrix
repr_matrix <- function(x, func = NULL, args = NULL, normalize = TRUE, func_norm = norm_z) {

  if (is.null(func)) {
    stop("func must be specified!")
  }

  x <- data.matrix(x)

  if (is.null(args)) {
    repr <- t(apply(x, 1, func))
  } else {
    repr <- t(sapply(1:nrow(x), function(i) do.call(func, args = append(list(x = x[i,]), args))))
  }

  if (normalize == TRUE) {
    repr <- t(apply(repr, 1, func_norm))
  }

  return(repr)
}
