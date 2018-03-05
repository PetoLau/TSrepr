context("Tests for normalisations functions");

# Lengths of outputted vectors
x_ts <- rep(1:8, 12)
test_that("Test on x_ts, length of output from selected functions", {
  expect_length(norm_z(x_ts), length(x_ts))
  expect_length(norm_min_max(x_ts), length(x_ts))
  expect_length(denorm_z(x_ts, 1, 1), length(x_ts))
  expect_length(denorm_min_max(x_ts, 1, 1), length(x_ts))
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
})
