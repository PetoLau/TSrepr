context("Tests for for. helper functions");

# Lengths of outputted values
x_ts <- rep(1:8, 12)
test_that("Test on x_ts, length of output from selected functions", {
  expect_length(meanC(x_ts), 1)
  expect_length(medianC(x_ts), 1)
  expect_length(maxC(x_ts), 1)
  expect_length(minC(x_ts), 1)
  expect_length(sumC(x_ts), 1)
})

x_ts_2 <- c(1:9)
# Extracted values
test_that("Test on x_ts, outputted values from helper functions", {
  expect_equal(meanC(x_ts), mean(x_ts))
  expect_equal(medianC(x_ts), median(x_ts))
  expect_equal(medianC(x_ts_2), median(x_ts_2)) # odd number testing
  expect_equal(sumC(x_ts), sum(x_ts))
  expect_equal(minC(x_ts), min(x_ts))
  expect_equal(maxC(x_ts), max(x_ts))
})
