context("Tests for data adaptive representations (PLA, SAX) and PIP functions");

# Lengths of outputted representations
x_ts <- rep(1:8, 12)
times <- 12
q <- 8
a <- 4
test_that("Test on x_ts, length of output from selected repr_...() functions", {
  expect_length(repr_pla(x_ts, times = times), times + 1)
  expect_length(repr_pip(x_ts, times = times), times + 1)
  expect_length(repr_sax(x_ts, q = q, a = a), length(x_ts)/q)
})

# Extracted values (repr.) testing
test_that("Test on x_ts, extracted values from repr_...() functions", {
  expect_equal(repr_pla(x_ts, times = times), c(x_ts[1], rep(max(x_ts), times)))
  expect_equal(repr_pip(x_ts, times = times)[1], x_ts[1])
  expect_equal(repr_pip(x_ts, times = times)[times+1], tail(x_ts, 1))
})

# Test errors
test_that("Test on x_ts, errors on selected repr_...() functions", {
  expect_error(repr_pla(x_ts, times = 1), "times must be at least 2!")
  expect_error(repr_pip(x_ts, times = 1), "times must be at least 2!")
  expect_error(repr_pla(x_ts, times = length(x_ts)), "times must be less than the length of x!")
  expect_error(repr_pip(x_ts, times = length(x_ts)), "times must be less than the length of x!")
})
