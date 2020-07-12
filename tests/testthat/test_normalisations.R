context("Tests for normalisations functions");

# Lengths of outputted vectors
x_ts <- rep(1:8, 12)

test_that("Test on x_ts, length of output from selected functions", {
  expect_length(norm_z(x_ts), length(x_ts))
  expect_length(norm_min_max(x_ts), length(x_ts))
  expect_length(norm_atan(x_ts), length(x_ts))
  expect_length(norm_boxcox(x_ts), length(x_ts))
  expect_length(norm_boxcox(x_ts, lambda = 0), length(x_ts))
  expect_length(norm_yj(c(x_ts, -5)), length(x_ts)+1)
  expect_length(norm_yj(c(x_ts, -5), lambda = 2), length(x_ts)+1)
  expect_length(norm_yj(c(x_ts, -5), lambda = 0), length(x_ts)+1)
  expect_length(denorm_z(x_ts, 1, 1), length(x_ts))
  expect_length(denorm_min_max(x_ts, 1, 1), length(x_ts))
  expect_length(denorm_atan(x_ts), length(x_ts))
  expect_length(denorm_boxcox(x_ts), length(x_ts))
  expect_length(denorm_yj(x_ts), length(x_ts))
  expect_length(denorm_boxcox(x_ts, lambda = 0), length(x_ts))
  expect_length(denorm_yj(c(x_ts, -5)), length(x_ts)+1)
  expect_length(denorm_yj(c(x_ts, -5), lambda = 2), length(x_ts)+1)
  expect_length(denorm_yj(c(x_ts, -5), lambda = 0), length(x_ts)+1)
  expect_length(norm_z_params(x_ts, 1, 2), length(x_ts))
  expect_length(norm_min_max_params(x_ts, 1, 8), length(x_ts))
})

# norm list lengths testing
test_that("Test on x_ts, length of norm_..._list() outputs", {
  expect_length(norm_z_list(x_ts), 3)
  expect_length(norm_min_max_list(x_ts), 3)
})

# Extracted values (repr.) testing
test_that("Test on x_ts, outputted values from norm_...() functions", {
  expect_equal(unique(norm_z(rep(5, 50))), 0)
  expect_equal(unique(norm_min_max(rep(5, 50))), 0)
  expect_equal(unique(norm_z_list(rep(5, 50))$norm_values), 0)
  expect_equal(unique(norm_min_max_list(rep(5, 50))$norm_values), 0)
  expect_equal(unique(norm_z_params(x_ts, 0, 0)), 0)
  expect_equal(unique(norm_min_max_params(x_ts, 0, 0)), 0)
})

# Test errors
test_that("Test on the same lengths of real and forecast values on acc. measures functions", {
  expect_error(norm_boxcox(c(-5:0), gamma = 2), "set gamma parameter higher to be x > 0")
  expect_error(norm_boxcox(c(0:5), gamma = -2), "gamma must be non-negative")
  expect_error(denorm_boxcox(c(0:5), gamma = -2), "gamma must be non-negative")
})
