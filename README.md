# TSrepr
 TSrepr: R package for fast time series representations and dimensionality reduction computations. Z-score normalisation, min-max normalisation, forecasting accuracy measures and other useful functions programmed in C++.

Install `TSrepr` R package via GitHub:

`
devtools::install_github("petolau/TSrepr")
`

These representations of time series are implemented so far:
 * PAA - Piecewise Aggregate Approximation
 * DWT - Discrete Wavelet Transform
 * DFT - Discrete Fourier Transform
 * DCT - Discrete Cosine Transform
 * SMA - Simple Moving Average
 * FeaClip - Feature extraction from clipping representation
 * FeaTrend - Feature extraction from trending representation
 * FeaClipTrend - Feature extraction from clipping and trending representation
 * Mean seasonal profile
 * Model-based seasonal representations based on linear (additive) model (lm, rlm, l1, gam)
 * Exponential smoothing seasonal coefficients
