# Non-data adaptive methods of representation of time series ----
# devtools::use_package("wavelets")

# DWT

#' @rdname repr_dwt
#' @name repr_dwt
#' @title DWT representation
#'
#' @description \code{repr_dwt} computes DWT representation (coefficients) from a time series.
#'
#' @return Numeric vector of DWT coefficients
#'
#' @param x Numeric vector
#' @param level level of DWT transformation (default is 4)
#' @param filter filter name (default is "haar")
#'
#' @seealso \code{\link[TSrepr]{repr_dft}, \link[TSrepr]{repr_dct}}
#'
#' @examples
#' repr_dwt(rnorm(50), level = 4)
#'
#' @export repr_dwt
repr_dwt <- function(x, level = 4, filter = "haar") {

  x <- as.numeric(x)

  repr <- wavelets::dwt(x, filter = filter, n.levels = level, boundary = "periodic")@V[[level]]

  return(as.vector(repr))
}

# DFT

#' @rdname repr_dft
#' @name repr_dft
#' @title DFT representation by FFT
#'
#' @description \code{repr_dft} computes DFT representation from a time series by FFT.
#'
#' @return Numeric vector of DFT coefficients
#'
#' @param x Numeric vector
#' @param coef Number of coefficients to extract
#'
#' @seealso \code{\link[TSrepr]{repr_dwt}, \link[TSrepr]{repr_dct}}
#'
#' @examples
#' repr_dft(rnorm(50), coef = 4)
#'
#' @export repr_dft
repr_dft <- function(x, coef) {

  x <- as.numeric(x)

  fourier_fft <- fft(x)
  inv_fft <- fft(fourier_fft[1:coef], inverse = TRUE) / coef

  return(as.vector(Re(inv_fft)))
}

# DCT

#' @rdname repr_dct
#' @name repr_dct
#' @title DCT representation
#'
#' @description \code{repr_dct} computes DCT representation from a time series.
#'
#' @return Numeric vector of DCT coefficients
#'
#' @param x Numeric vector
#' @param coef Number of coefficients to extract
#'
#' @seealso \code{\link[TSrepr]{repr_dft}, \link[TSrepr]{repr_dwt}}
#'
#' @examples
#' repr_dct(rnorm(50), coef = 4)
#'
#' @export repr_dct
repr_dct <- function(x, coef) {

  x <- as.numeric(x)

  x_dct <- dtt::dct(x)
  repr <- dtt::dct(x_dct[1:coef], inverted = TRUE)

  return(as.vector(repr))
}
