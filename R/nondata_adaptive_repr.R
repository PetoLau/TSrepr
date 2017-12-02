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
#' @param filter the filter name (default is "d6").
#'  Can be: "haar", "d4", "d6", ..., "d20", "la8", "la10", ..., "la20", "bl14", "bl18", "bl20",
#'   "c6", "c12", ..., "c30". See more info at \code{\link[wavelets]{wt.filter}}.
#'
#' @details This function extracts DWT coefficients.
#' You can use various wavelet filters, see all of them here \code{\link[wavelets]{wt.filter}}.
#' The number of extracted coefficients depends on the \code{level} selected.
#' The final representation has length equal to floor(n / 2^{level}), where n is a length of original time series.
#'
#' @seealso \code{\link[TSrepr]{repr_dft}, \link[TSrepr]{repr_dct}, \link[wavelets]{dwt}}
#'
#' @author Peter Laurinec, <tsreprpackage@gmail.com>
#'
#' @references Laurinec P, Lucka M (2016)
#' Comparison of representations of time series for clustering smart meter data.
#' In: Lecture Notes in Engineering and Computer Science: Proceedings of The World Congress on Engineering and Computer Science 2016, pp 458-463
#'
#' @examples
#' # Interpretation: DWT with Daubechies filter of length 4 and
#' # 3rd level of DWT coefficients extracted.
#' repr_dwt(rnorm(50), filter = "d4", level = 3)
#'
#' @importFrom wavelets dwt
#' @export repr_dwt
repr_dwt <- function(x, level = 4, filter = "d4") {

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
#' @details The length of the final time series representation is equal to set \code{coef} parameter.
#'
#' @seealso \code{\link[TSrepr]{repr_dwt}, \link[TSrepr]{repr_dct}, \link[stats]{fft}}
#'
#' @author Peter Laurinec, <tsreprpackage@gmail.com>
#'
#' @examples
#' repr_dft(rnorm(50), coef = 4)
#'
#' @importFrom stats fft
#' @export repr_dft
repr_dft <- function(x, coef = 10) {

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
#' @details The length of the final time series representation is equal to set \code{coef} parameter.
#'
#' @seealso \code{\link[TSrepr]{repr_dft}, \link[TSrepr]{repr_dwt}, \link[dtt]{dct}}
#'
#' @author Peter Laurinec, <tsreprpackage@gmail.com>
#'
#' @examples
#' repr_dct(rnorm(50), coef = 4)
#'
#' @importFrom dtt dct
#' @export repr_dct
repr_dct <- function(x, coef = 10) {

  x <- as.numeric(x)

  x_dct <- dtt::dct(x)
  repr <- dtt::dct(x_dct[1:coef], inverted = TRUE)

  return(as.vector(repr))
}
