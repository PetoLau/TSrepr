# TSrepr
 TSrepr: R package for fast time series representations and dimensionality reduction computations. Z-score normalisation, min-max normalisation, forecasting accuracy measures and other useful functions programmed in C++.

Install `TSrepr` R package via GitHub:

`
devtools::install_github("petolau/TSrepr")
`

These representations of time series are implemented so far:
 * PAA
 * DWT
 * DFT
 * DCT
 * SMA
 * FeaClip
 * FeaTrend
 * FeaClipTrend
 * Mean seasonal profile
 * Model-based representations based on linear (additive) model (lm, rlm, l1, gam)
 * Exponential smoothing seasonal coefficients
