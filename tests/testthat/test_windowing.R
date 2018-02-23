context("Tests for repr_windowing() function");

# Lengths of outputted representation
x_ts <- rep(1:8, 12)
win_size <- 24
test_that("Test on x_ts, length of output from repr_windowing() function", {
  expect_length(repr_windowing(x_ts, win_size = win_size, func = repr_feaclip), (length(x_ts)/win_size) * 8)
  expect_length(repr_windowing(c(x_ts, 9), win_size = win_size, func = repr_feaclip), ceiling(((length(x_ts) + 1)/win_size)) * 8)
})

# Test errors
test_that("Test on x_ts, errors on repr_windowing() function", {
  expect_error(repr_windowing(x_ts, win_size = win_size), "func must be specified!")
})
