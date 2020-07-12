# Computation of list of representations from list of time series with different lengths

#' @rdname repr_list
#' @name repr_list
#' @title Computation of list of representations list of time series with different lengths
#'
#' @description The \code{repr_list} computes list of representations from list of time series
#'
#' @return the numeric list of representations of time series
#'
#' @param x the list of time series, where time series can have different lengths
#' @param func the function that computes representation
#' @param args the list of additional (or required) parameters of func (function that computes representation)
#' @param normalise normalise (scale) time series before representations computation? (default is FALSE)
#' @param func_norm the normalisation function (default is \code{norm_z})
#' @param windowing perform windowing? (default is FALSE)
#' @param win_size the size of the window
#'
#' @details This function computes representation to an every member of a list of time series (that can have different lengths) and returns list of time series representations.
#' It can be combined with windowing (see \code{\link{repr_windowing}}) and normalisation of time series.
#'
#' @author Peter Laurinec, <tsreprpackage@gmail.com>
#'
#' @seealso \code{\link[TSrepr]{repr_windowing}, \link[TSrepr]{repr_matrix}}
#'
#' @examples
#' # Create random list of time series with different lengths
#' list_ts <- list(rnorm(sample(8:12, 1)), rnorm(sample(8:12, 1)), rnorm(sample(8:12, 1)))
#' repr_list(list_ts, func = repr_sma,
#'  args = list(order = 3))
#'
#' # return normalised representations, and normalise time series by min-max normalisation
#' repr_list(list_ts, func = repr_sma,
#'  args = list(order = 3), normalise = TRUE, func_norm = norm_min_max)
#'
#' @export repr_list
repr_list <- function(x, func = NULL, args = NULL, normalise = FALSE, func_norm = norm_z, windowing = FALSE, win_size = NULL) {

  if (is.null(func)) {

    stop("func must be specified!")

  }

  x <- as.list(x)

  if (normalise == TRUE) {

    x <- lapply(x, func_norm)

  }

  if (windowing) {

    if (is.null(win_size)) {

      stop("win_size must be specified!")

    }

    repr <- lapply(x, function(i) repr_windowing(i,
                                                 win_size = win_size,
                                                 func = func,
                                                 args = args
                                                 )
                   )

  } else {

    repr <- lapply(x, function(i) do.call(func, args = append(list(x = i),
                                                              args
                                                              )
                                          )
                   )

    }

  return(repr)

}
