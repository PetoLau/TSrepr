# ArcTan normalisation to (-1,1) range ----

#' @rdname norm_atan
#' @name norm_atan
#' @title Arctangent normalisation
#'
#' @description The \code{norm_atan} normalises time series by Arctangent to max (-1,1) range.
#'
#' @return the numeric vector of normalised values
#'
#' @param x the numeric vector (time series)
#'
#' @author Peter Laurinec, <tsreprpackage@gmail.com>
#'
#' @seealso \code{\link[TSrepr]{norm_z}, \link[TSrepr]{norm_min_max}}
#'
#' @examples
#' norm_atan(rnorm(50))
#'
#' @export norm_atan
norm_atan <- function(x) {

  x <- as.numeric(x)

  norm_values <- atan(x) / (pi / 2)


  return(norm_values)

}

#' @rdname denorm_atan
#' @name denorm_atan
#' @title Arctangent denormalisation
#'
#' @description The \code{denorm_atan} denormalises time series from Arctangent function.
#'
#' @return the numeric vector of denormalised values
#'
#' @param x the numeric vector (time series)
#'
#' @author Peter Laurinec, <tsreprpackage@gmail.com>
#'
#' @seealso \code{\link[TSrepr]{denorm_z}, \link[TSrepr]{denorm_min_max}}
#'
#' @examples
#' denorm_atan(runif(50))
#'
#' @export denorm_atan
denorm_atan <- function(x) {

  x <- as.numeric(x)

  denorm_values <- tan(x * (pi / 2))

  return(denorm_values)

}

# Two-parameter Box-Cox normalisation -----

#' @rdname norm_boxcox
#' @name norm_boxcox
#' @title Two-parameter Box-Cox normalisation
#'
#' @description The \code{norm_boxcox} normalises time series by two-parameter Box-Cox normalisation.
#'
#' @return the numeric vector of normalised values
#'
#' @param x the numeric vector (time series)
#' @param lambda the numeric value - power transformation parameter (default is 0.1)
#' @param gamma the non-negative numeric value - parameter for holding the time series positive (offset) (default is 0)
#'
#' @author Peter Laurinec, <tsreprpackage@gmail.com>
#'
#' @seealso \code{\link[TSrepr]{norm_z}, \link[TSrepr]{norm_min_max}, \link[TSrepr]{norm_atan}}
#'
#' @examples
#' norm_boxcox(runif(50))
#'
#' @export norm_boxcox
norm_boxcox <- function(x, lambda = 0.1, gamma = 0) {

  x <- as.numeric(x)

  if (gamma < 0) {

    stop("gamma must be non-negative")

  } else if (lambda <= 0 & sum(x == 0L) > 0) {

    stop("set gamma parameter higher to be x > 0")

  }

  if (lambda == 0) {

    norm_values <- log(x + gamma)

  } else {

    norm_values <- (((x + gamma) ^ lambda) - 1) / lambda

  }

  return(norm_values)

}

# TODO denorm_boxcox

# Yeo-Johnson normalisation -----

# TODO norm_yj, denorm_yj
