# Non-data adaptive methods of representation of time series ----
# devtools::use_package("wavelets")

# DWT

#' @rdname repr_dwt
#' @name repr_dwt
#' @title DWT representation
#'
#' @description The \code{repr_dwt} computes DWT (Discrete Wavelet Transform) representation (coefficients) from a time series.
#'
#' @return the numeric vector of DWT coefficients
#'
#' @param x the numeric vector (time series)
#' @param level the level of DWT transformation (default is 4)
#' @param filter the filter name (default is "haar")
#'
#' @seealso \code{\link[TSrepr]{repr_dft}, \link[TSrepr]{repr_dct}}
#'
#' @examples
#' repr_dwt(rnorm(50), level = 4)
#'
#' @importFrom wavelets dwt
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
#' @description The \code{repr_dft} computes DFT (Discrete Fourier Transform) representation from a time series by FFT (Fast Fourier Transform).
#'
#' @return the numeric vector of DFT coefficients
#'
#' @param x the numeric vector (time series)
#' @param coef the number of coefficients to extract from FFT
#'
#' @seealso \code{\link[TSrepr]{repr_dwt}, \link[TSrepr]{repr_dct}}
#'
#' @examples
#' repr_dft(rnorm(50), coef = 4)
#'
#' @importFrom stats fft
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
#' @description The \code{repr_dct} computes DCT (Discrete Cosine Transform) representation from a time series.
#'
#' @return the numeric vector of DCT coefficients
#'
#' @param x the numeric vector (time series)
#' @param coef the number of coefficients to extract from DCT
#'
#' @seealso \code{\link[TSrepr]{repr_dft}, \link[TSrepr]{repr_dwt}}
#'
#' @examples
#' repr_dct(rnorm(50), coef = 4)
#'
#' @importFrom dtt dct
#' @export repr_dct
repr_dct <- function(x, coef) {

  x <- as.numeric(x)

  x_dct <- dtt::dct(x)
  repr <- dtt::dct(x_dct[1:coef], inverted = TRUE)

  return(as.vector(repr))
}
