context("Tests for for. acc. measures functions");

# Lengths of outputted values
real <- rep(1:8, 12)
forec <- rep(8:1, 12)
naive <- rep(9:2, 12)

test_that("Test on real and forec, length of output from selected functions", {
  expect_length(mse(real, forec), 1)
  expect_length(mae(real, forec), 1)
  expect_length(rmse(real, forec), 1)
  expect_length(mdae(real, forec), 1)
  expect_length(mape(real, forec), 1)
  expect_length(smape(real, forec), 1)
  expect_length(maape(real, forec), 1)
  expect_length(mase(real, forec, naive), 1)
})

# Extracted values
test_that("Test on real and forec, outputted values from acc. measures functions", {
  expect_equal(mae(c(4,5), c(2,4)), 1.5)
  expect_equal(mse(c(4,5), c(2,4)), 2.5)
  expect_equal(mdae(c(4,5), c(2,4)), 1.5)
  expect_equal(mape(c(4,5), c(2,4)), 35)
  expect_equal(maape(0, 1), pi/2)
  # expect_equal(smape(c(4,5), c(2,4)), 11111/250)
  expect_equal(mase(c(4,5), c(2,4), c(10,8)), 1/3)
})
