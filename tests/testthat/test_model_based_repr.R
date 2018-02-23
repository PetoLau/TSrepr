context("Tests for model-based representations functions");

# Lengths of outputted representations
x_ts <- rep(1:8, 48)
freq <- 24
freq_2 <- freq*4
test_that("Test on x_ts, length of output from selected repr_...() functions on one season", {
  expect_length(repr_lm(x_ts, freq = freq, method = "lm"), freq)
  expect_length(repr_lm(x_ts, freq = freq, method = "rlm"), freq)
  expect_length(repr_lm(x_ts, freq = freq, method = "l1"), freq)
  expect_length(repr_gam(x_ts, freq = freq), freq - 1)
  expect_length(repr_exp(x_ts, freq = freq), freq)
})

test_that("Test on x_ts, length of output from selected repr_...() functions on two seasonalities", {
  expect_length(repr_lm(x_ts, freq = c(freq, freq_2), method = "lm"), freq + (freq_2)/freq - 1)
  expect_length(repr_lm(x_ts, freq = c(freq, freq_2), method = "rlm"), freq + (freq_2)/freq - 1)
  expect_length(repr_lm(x_ts, freq = c(freq, freq_2), method = "l1"), freq + (freq_2)/freq - 1)
  expect_length(repr_gam(x_ts, freq = c(freq, freq_2)), freq + (freq_2)/freq - 2)
})

# Extracted values (repr.) testing
test_that("Test on x_ts, extracted values from model-based repr_...() functions", {
  expect_equal(mean(repr_lm(x_ts, freq = freq, method = "lm")), mean(x_ts))
  expect_equal(mean(repr_lm(x_ts, freq = freq, method = "rlm")), mean(x_ts))
  expect_equal(mean(repr_lm(x_ts, freq = freq, method = "l1")), mean(x_ts))
})

# Test errors
test_that("Test on x_ts, errors on model-based repr_...() functions", {
  expect_error(repr_lm(x_ts, freq = c(freq, freq, freq), method = "lm"), "Number of seasonalities must be less than 3!")
  expect_error(repr_lm(x_ts, freq = c(freq_2, freq), method = "lm"), "First seasonality must be less than second one!")
  expect_error(repr_gam(x_ts, freq = c(freq, freq, freq)), "Number of seasonalities must be less than 3!")
  expect_error(repr_gam(x_ts, freq = c(freq_2, freq)), "First seasonality must be less than second one!")
})
