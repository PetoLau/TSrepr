# Computation of matrix of representations from matrix of time series

#' @rdname repr_matrix
#' @name repr_matrix
#' @title Computation of matrix of representations from matrix of time series
#'
#' @description \code{repr_matrix} Computation of matrix of representations from matrix of time series
#'
#' @return Numeric matrix of representations of time series
#'
#' @param x matrix, data.frame or data.table of time series, where time series are in rows of a table
#' @param fun function that computes representation
#' @param args list of additional (or required) parameters of fun (function that computes representation)
#' @param normalize normalize time series before computation of representations? (default TRUE)
#' @param fun_norm normalization function (default \code{norm_z})
#'
#' @examples
#' # Create random matrix of time series
#' mat_ts <- matrix(rnorm(100), ncol = 10)
#' repr_matrix(mat_ts, fun = repr_paa, args = list(q = 5, func = meanC))
#'
#' @export
repr_matrix <- function(x, fun = NULL, args = NULL, normalize = TRUE, fun_norm = norm_z) {

  if (is.null(fun)) {
    stop("fun must be specified!")
  }

  x <- data.matrix(x)

  if (normalize == TRUE) {
    x <- t(apply(x, 1, fun_norm))
  }

  if (is.null(args)) {
    repr <- t(apply(x, 1, fun))
  } else {
    repr <- t(sapply(1:nrow(x), function(i) do.call(fun, args = append(list(x = x[i,]), args))))
  }

  return(repr)
}
