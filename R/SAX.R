#' @rdname repr_sax
#' @name repr_sax
#' @title SAX - Symbolic Aggregate Approximation
#'
#' @description The \code{repr_sax} creates SAX symbols for a univariate time series.
#'
#' @param x the numeric vector (time series)
#' @param q the integer of the length of the "piece" in PAA
#' @param a the integer of the alphabet size
#' @param eps is the minimum threshold for variance in x and should be a numeric value. If x has a smaller variance than eps, it will represented as a word using the middle alphabet.
#'
#' @return the character vector of SAX representation
#'
#' @seealso \code{\link[TSrepr]{repr_paa}, \link[TSrepr]{repr_pla}}
#'
#' @author Peter Laurinec, <tsreprpackage@gmail.com>
#'
#' @references Lin J, Keogh E, Lonardi S, Chiu B (2003)
#' A symbolic representation of time series, with implications for streaming algorithms.
#' Proceedings of the 8th ACM SIGMOD Workshop on Research Issues in Data Mining and Knowledge Discovery - DMKD'03
#'
#' @importFrom stats sd
#' @importFrom stats qnorm
#'
#' @examples
#' x <- rnorm(48)
#' repr_sax(x, q = 4, a = 5)
#'
#' @export repr_sax
repr_sax <- function(x, q = 2, a = 6, eps = 0.01) {

  x <- as.numeric(x)

  if (sd(x) <= eps) {
    repr <- rep(letters[round((1+a)/2, digits = 0)], ceiling(length(x)/q))
    } else {

    # Perform the PAA
    pieces <- repr_paa(x, q = q, func = meanC)

    # Perform alphabet assignment
    let <- letters[1:a]
    # Create breaks points based on Gaussian normal distribution
    bks <- round(qnorm(p = seq(from = 0, to = 1, length.out = a+1)), digits = 5)[2:a]
    repr <- sapply(1:length(pieces), function(i) let[max(which(bks < pieces[i]))])
  }

  return(repr)
}
