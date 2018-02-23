# PLA - Piecewise Linear Approximation ----
# code based on: https://gist.github.com/ionescuv/63fdace1dda266ae89c7

toSlopes <- function(ts) {

  slopes <- diff(ts)
  indexes <- 0:(length(slopes)-1)
  aux <- slopes * indexes
  offset <- ts[1:length(slopes)] - aux
  lngth <- rep(1, length(slopes))

  res <- data.frame(matrix(c(slopes, offset, indexes, lngth), ncol = 4))
  colnames(res) <- c("a", "b", "startp", "runlength")

  return(res)
}

fValue <- function(segs, x) {
  return(segs[1, "a"]*x + segs[1, "b"])
}

mergeSegs <- function(segs) {

  slope <- weighted.mean(segs[, "a"], segs[, "runlength"])
  start <- segs[1, "startp"]
  length <- sum(segs[, "runlength"])

  if (segs[1,"a"] == segs[2, "a"]) {
    offset <- segs[1,"b"]
  } else {
    offset <- fValue(segs[1,], segs[1, "startp"]) - (slope * segs[1, "startp"])
  }

  res <- data.frame(slope, offset, start, length)
  colnames(res) <- c("a", "b", "startp", "runlength")
  return(res)
}

segdif <- function(segs, x1, x2) {
  a1 <- segs[1, "a"]
  a2 <- segs[2, "a"]
  b1 <- segs[1, "b"]
  b2 <- segs[2, "b"]

  out <- (a1 - a2)*(x2*x2 - x1*x1)/2 + (b1 - b2)*(x2 - x1)
  return(out)
}

mergeCost <- function(segs) {

  merged <- mergeSegs(segs)

  start1 <- segs[1, "startp"]
  end1 <- start1 + segs[1,"runlength"]

  cost <- segdif(rbind(merged, segs[1,]), start1, end1)

  start2 <- segs[2, "startp"]
  end2 <- start2 + segs[2, "runlength"]

  cost <- cost + segdif(rbind(merged, segs[2,]), start2, end2)

  return(abs(cost))
}

costVector <- function(segs) {
  start <- 0
  cost <- NULL
  ## determine merge cost for each pair of segments

  for (i in 1:(length(segs[,1])-1))
  {
    cost[i] <- mergeCost(segs[i:(i+1),])
  }
  return(cost)
}

bottomUp <- function(segs, minIndex) {

  ## now adjust the data
  out <- segs
  out[minIndex,] <- mergeSegs(out[minIndex:(minIndex+1),])
  out <- out[-c(minIndex+1),]

  return(out)
}

bottomUpCost <- function(segs, cost, minIndex) {

  ## now adjust the data
  out <- cost

  if (minIndex < length(segs[,"a"]))
  {
    out <- out[-c(minIndex+1)] # eliminate cost for entry that is being deleted
    out[minIndex] <- mergeCost(segs[minIndex:(minIndex+1),]) #recalculate cost after merge
  }
  else ## special case when we are eliminating the last entry
  {
    out <- out[-c(minIndex)] # eliminate last cost entry
  }

  return(out)
}

segment <- function(ts, segments) {
  slopes <- toSlopes(ts)
  cost <- costVector(slopes)

  while(length(slopes[,"a"]) > segments) {
    ## find minimum merge cost
    minIndex <- which.min(cost)

    ## now adjust the data
    slopes <- bottomUp(slopes, minIndex)
    cost <- bottomUpCost(slopes, cost, minIndex)
  }

  return(slopes)
}

#### helper function for display ####

segPlot <- function(segs) {

  x <- 0
  vec <- c(x, fValue(segs[1,], x))

  for(i in 1:length(segs[, "a"])){
    x <- x + segs[i, "runlength"]
    vec <- rbind(vec, c(x, fValue(segs[i,], x)))
  }

  return(vec)
}

#' @rdname repr_pla
#' @name repr_pla
#' @title PLA representation
#'
#' @description The \code{repr_pla} computes PLA (Piecewise Linear Approximation) representation from a time series.
#'
#' @return the values based on the argument return (see above)
#'
#' @param x the numeric vector (time series)
#' @param times the number of important points to extract (default 10)
#' @param return what to return? Can be "points" (segments),
#'  places of points (segments) in a vector ("places") or "both" (data.frame).
#'
#' @author Peter Laurinec, <tsreprpackage@gmail.com>
#'
#' @references Zhu Y, Wu D, Li Sh (2007)
#' A Piecewise Linear Representation Method of Time Series Based on Feature Points.
#' Knowledge-Based Intelligent Information and Engineering Systems 4693:1066-1072
#'
#' @examples
#' repr_pla(rnorm(100), times = 12, return = "both")
#'
#' @importFrom stats weighted.mean
#' @export repr_pla
repr_pla <- function(x, times = 10, return = "points") {

  if (times <= 1) {
    stop("times must be at least 2!")
  }

  if (length(x) <= times) {
    stop("times must be less than the length of x!")
  }

  x <- as.numeric(x)

  repr <- segPlot(segment(x, times))
  row.names(repr) <- NULL

  if (return == "points") {
    return(repr[, 2])
  } else if (return == "places") {
    return(repr[, 1])
  } else {
    return(data.frame(places = repr[, 1],
                      points = repr[, 2]))
  }

}
