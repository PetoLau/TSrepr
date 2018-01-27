---
title: 'TSrepr R package: Time Series Representations'
tags:
- time series
- data mining
authors:
- name: Peter Laurinec
  orcid: 0000-0002-3501-8783
  affiliation: 1
affiliations:
- name: Faculty of Informatics and Information Technologies,
    Slovak University of Technology in Bratislava
  index: 1
date: 27 January 2018
bibliography: paper.bib
output: pdf_document
---

# Summary

TSrepr [@tsrepr] is R package for time series representations computing. Time series representations are, in other words, methods for dimensionality reduction, feature extraction or preprocessing of time series. They are used for:

 * significant reduction of the time series dimensionality
 * emphasis on fundamental (essential) shape characteristics
 * implicit noise handling
 * reducing the dimension will reduce memory requirements and computational complexity of consequent machine learning methods (classification or clustering).

The TSrepr package contains various methods and types of time series representations including Piecewise Aggregate Approximation (PAA), Discrete Fourier Transform (DFT), Perceptually Important Points (PIP), Symbolic Aggregate approXimation (SAX), Piecewise Linear Approximation (PLA) or Clipping. Except for these well-known methods, additional methods suitable for seasonal time series are implemented. These methods are based on the model as for example multiple linear regression, robust regression, generalized additive model or triple exponential smoothing [@Lau]. Own feature extraction methods from Clipping representation are also implemented - FeaClip and FeaTrend.

Additional useful functions and methods related to time series representations were also implemented. Package includes functions for normalisations and denormalisations of time series - z-score and min-max methods. It supports simple computation of windowing method, a matrix of representations and forecasting accuracy measures (MAE, RMSE, MAPE, sMAPE etc.).

Methods (functions) were implemented in base R and also in C++ for fast computations. In R, C++ programmes can be written thanks to the package Rcpp [@rcpp].

The general package for time series representations computations missed until now.
The CRAN's time series task view proves previous statement. Packages TSMining [@tsmining] and jmotif [@jmotif] both includes implementations of PAA and SAX time series representations methods. However, these packages are mainly focused on motif discovery in time series.

# References
