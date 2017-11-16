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
#' @param normalise normalise (scale) representations? (default is FALSE)
#' @param func_norm the normalization function (default is \code{norm_z})
#' @param windowing perform windowing? (default is FALSE)
#' @param win_size the size of the window
#'
#' @details TODO.
#'
#' @seealso \code{\link[TSrepr]{repr_windowing}}
#'
#' @examples
#' # Create random matrix of time series
#' mat_ts <- matrix(rnorm(100), ncol = 10)
#' repr_matrix(mat_ts, func = repr_paa,
#'  args = list(q = 5, func = meanC))
#'
#' # return normalised representations, and normalise time series by min-max normalisation
#' repr_matrix(mat_ts, func = repr_paa,
#'  args = list(q = 2, func = meanC), normalise = TRUE, func_norm = norm_min_max)
#'
#' # with windowing
#' repr_matrix(mat_ts, func = repr_feaclip, windowing = TRUE, win_size = 5)
#'
#' @export repr_matrix
repr_matrix <- function(x, func = NULL, args = NULL, normalise = FALSE, func_norm = norm_z, windowing = FALSE, win_size = NULL) {

  if (is.null(func)) {
    stop("func must be specified!")
  }

  x <- data.matrix(x)

  if (windowing) {

    if (is.null(win_size)) {
      stop("win_size must be specified!")
    }

    repr <- t(sapply(1:nrow(x), function(i) do.call(repr_windowing, args = append(list(x = x[i,]),
                                                                                  append(list(func = func,
                                                                                       win_size = win_size),
                                                                                       args)
                                                                                  ))))
  } else {
    repr <- t(sapply(1:nrow(x), function(i) do.call(func, args = append(list(x = x[i,]),
                                                                        args))))
  }

  # if (is.null(args)) {
  #   repr <- t(apply(x, 1, func))
  # } else {
  #   repr <- t(sapply(1:nrow(x), function(i) do.call(func, args = append(list(x = x[i,]), args))))
  # }

  if (normalise == TRUE) {
    repr <- t(apply(repr, 1, func_norm))
  }

  return(repr)
}
