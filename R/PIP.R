# PIP (Perceptually Important Points) ----
# Code by @kevin: https://stackoverflow.com/questions/30428900/efficient-perceptually-important-points-pips-algo-in-r-or-rcpp
# little bit adjusted inputs and outputs of functions

pip <- function(ts, interp = NULL, pips = NULL) {
  if (missing(interp)) {
    interp <- approx(x = c(ts[1,"x"], ts[nrow(ts),"x"]), y = c(ts[1,"y"], ts[nrow(ts),"y"]), n = nrow(ts))
    interp <- do.call(cbind, interp)
    pips <- c(1, nrow(ts))
  }
  else {
    distances <- sqrt(rowSums((ts - interp)^2))  # close by euclidean distance
    if (sum(distances) == 0) {
      newPIP <- which(!(ts[, 1] %in% pips))[1]
    } else {
      newPIP <- which.max(distances)
      }
    adjacentPIPs <- c(min(newPIP-pips[pips<newPIP]), min(pips[pips>newPIP]-newPIP))

    line1 <- approx(x = c(ts[newPIP-adjacentPIPs[1],"x"], ts[newPIP,"x"]), y = c(ts[newPIP-adjacentPIPs[1],"y"], ts[newPIP,"y"]), n = adjacentPIPs[1]+1)
    line2 <- approx(x = c(ts[newPIP,"x"], ts[newPIP+adjacentPIPs[2],"x"]), y = c(ts[newPIP,"y"], ts[newPIP+adjacentPIPs[2],"y"]), n = adjacentPIPs[2]+1)
    interp[(newPIP-adjacentPIPs[1]):newPIP, "y"] <- line1$y
    interp[(newPIP):(newPIP+adjacentPIPs[2]), "y"] <- line2$y
    pips <- c(pips, newPIP)
  }
  return(list(interp = interp, pips = pips))
}

#' @rdname repr_pip
#' @name repr_pip
#' @title PIP representation
#'
#' @description The \code{repr_pip} computes PIP (Perceptually Important Points) representation from a time series.
#'
#' @return the values based on the argument return (see above)
#'
#' @param x the numeric vector (time series)
#' @param times the number of important points to extract (default 10)
#' @param return what to return? Can be important points ("points"),
#'  places of important points in a vector ("places") or "both" (data.frame).
#'
#' @examples
#' repr_pip(rnorm(100), times = 12, return = "both")
#'
#' @importFrom stats approx
#' @export repr_pip
repr_pip <- function(x, times = 10, return = "points") {

  if (times <= 1) {
    stop("times must be at least 2!")
  }

  if (length(x) <= times) {
    stop("times must be less than the length of x!")
  }

  mat_ts <- as.matrix(data.frame(x = 1:length(x), y = as.numeric(x)))

  res <- pip(mat_ts)
  for (i in 2:times) {
    res <- pip(mat_ts, res$interp, res$pips)
  }

  if (return == "points") {
    return(mat_ts[sort(res$pips), 2])
  }

  if (return == "places") {
    return(sort(res$pips))
  }

  if (return == "both") {
    return(data.frame(places = sort(res$pips),
                      points = mat_ts[sort(res$pips), 2]))
  }

}
