#' TSrepr package
#'
#' Package contains methods for time series representations computation.
#' Representation methods of time series are for dimensionality and noise reduction,
#' emphasizing of main characteristics of time series data and speed up of consequent usage of machine learning methods.
#'
#' \tabular{ll}{ Package: \tab TSrepr\cr Type: \tab Package\cr Date: \tab
#' 2017-11-23 - Inf\cr License: \tab GPL-3 \cr }
#' The following functions for time series representations are included in the package:
#' \itemize{
#' \item \link[TSrepr]{repr_paa} - Piecewise Aggregate Approximation (PAA)
#' \item \link[TSrepr]{repr_dwt} - Discrete Wavelet Transform (DWT)
#' \item \link[TSrepr]{repr_dft} - Discrete Fourier Transform (DFT)
#' \item \link[TSrepr]{repr_dct} - Discrete Cosine Transform (DCT)
#' \item \link[TSrepr]{repr_sma} - Simple Moving Average (SMA)
#' \item \link[TSrepr]{repr_pip} - Perceptually Important Points (PIP)
#' \item \link[TSrepr]{repr_pla} - Piecewise Linear Approximation (PLA)
#' \item \link[TSrepr]{repr_seas_profile} - Mean seasonal profile
#' \item \link[TSrepr]{repr_lm} - Model-based seasonal representations based on linear model (lm, rlm, l1)
#' \item \link[TSrepr]{repr_gam} - Model-based seasonal representations based on generalized additive model (GAM)
#' \item \link[TSrepr]{repr_exp} - Exponential smoothing seasonal coefficients
#' \item \link[TSrepr]{repr_feaclip} - Feature extraction from clipping representation (FeaClip)
#' \item \link[TSrepr]{repr_featrend} - Feature extraction from trending representation (FeaTrend)
#' \item \link[TSrepr]{repr_feacliptrend} - Feature extraction from clipping and trending representation (FeaClipTrend)
#' }
#' There are also implemented additional useful functions as:
#' \itemize{
#' \item \link[TSrepr]{repr_windowing} - applies above mentioned representations to every window of a time series
#' \item \link[TSrepr]{repr_matrix} - applies above mentioned representations to every row of a matrix of time series
#' \item \link[TSrepr]{norm_z}, \link[TSrepr]{norm_min_max} - normalisation functions
#' \item \link[TSrepr]{norm_z_list}, \link[TSrepr]{norm_min_max_list} - normalisation functions with output also of scaling parameters
#' \item \link[TSrepr]{denorm_z}, \link[TSrepr]{denorm_min_max} - denormalisation functions
#' }
#'
#' @name TSrepr
#' @docType package
#' @author Peter Laurinec
#'
#' Maintainer: Peter Laurinec <tsreprpackage@gmail.com>
#'
#' @useDynLib TSrepr
NULL
