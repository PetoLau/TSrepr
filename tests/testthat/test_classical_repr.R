context("Tests for classical representations (SMA, PAA, seasonal profile) functions");

# Lengths of outputted representations
x_ts <- rep(1:8, 12)
order <- 5
q <- 8
freq <- 24
test_that("Test on x_ts, length of output from selected repr_...() functions", {
  expect_length(repr_sma(x_ts, order = order), length(x_ts) - order)
  expect_length(repr_paa(x_ts, q = q, func = mean), length(x_ts)/q)
  expect_length(repr_paa(x_ts[-1], q = q, func = mean), ceiling(length(x_ts[-1])/q))
  expect_length(repr_seas_profile(x_ts, freq = freq, func = mean), freq)
  expect_length(repr_seas_profile(c(8,5,x_ts), freq = freq, func = mean), freq)
})

# Extracted values (repr.) testing
test_that("Test on x_ts, extracted values from repr_...() functions", {
  expect_equal(mean(repr_sma(x_ts, order = order)), 4.2)
  expect_equal(unique(repr_paa(x_ts, q = q, func = mean)), mean(x_ts))
  expect_equal(repr_seas_profile(x_ts, freq = freq, func = mean), rep(1:8, 3))
})
