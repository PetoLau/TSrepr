# Non-data adaptive methods of representation of time series ----
# devtools::use_package("wavelets")

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
#' @seealso \code{\link[Tsrepr]{repr_dft}}
#'
#' @examples
#' repr_dwt(rnorm(50), level = 4)
#'
#' @export
repr_dwt <- function(x, level = 4, filter = "haar") {

  x <- as.numeric(x)

  repr <- wavelets::dwt(x, filter = filter, n.levels = level, boundary = "periodic")@V[[level]]

  return(as.vector(repr))
}

# Inverse FFT

#' @rdname fftinv
#' @name fftinv
#' @title Inverse FFT
#'
#' @description \code{fftinv} computes inverse FFT.
#'
#' @return Numeric vector
#'
#' @param x Numeric vector of Fourier coefficients
#'
#' @examples
#' fftinv(fft(rnorm(50))[1:10])
#'
#' @export
fftinv <- function(x) {
  fft(x, inverse = TRUE) / length(x)
}

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
#' @seealso \code{\link[Tsrepr]{repr_dwt}}
#'
#' @examples
#' repr_dft(rnorm(50), coef = 4)
#'
#' @export
repr_dft <- function(x, coef) {

  x <- as.numeric(x)

  fourier_fft <- fft(x)
  inv_fft <- fftinv(fourier_fft[1:coef])

  return(as.vector(Re(inv_fft)))
}
