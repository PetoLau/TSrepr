context("Tests for nondata adaptive representations functions");

# Lengths of outputted representations
x_ts <- rep(1:8, 12)
level <- 2
coef <- 8
test_that("Test on x_ts, length of output from selected repr_...() functions", {
  expect_length(repr_dwt(x_ts, level = level), length(x_ts)/(2^level))
  expect_length(repr_dft(x_ts, coef = coef), coef)
  expect_length(repr_dct(x_ts, coef = coef), coef)
})
