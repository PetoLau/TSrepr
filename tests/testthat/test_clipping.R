context("Tests for feature extraction from clipping repr. functions");

# Lengths of outputted representations
x_ts <- rep(1:8, 12)
pieces <- 4
test_that("Test on x_ts, length of output from selected functions", {
  expect_length(clipping(x_ts), length(x_ts))
  expect_length(trending(x_ts), length(x_ts) - 1)
  expect_length(repr_feaclip(x_ts), 8)
  expect_length(repr_featrend(x_ts, func = max, pieces = pieces), pieces*2)
  expect_length(repr_feacliptrend(x_ts, func = max, pieces = pieces), 8 + (pieces*2))
})

# RLE lengths testing
rle_o <- rleC(clipping(x_ts))
test_that("Test on x_ts, length of RLE outputs", {
  expect_length(rle_o, 2)
  expect_length(rle_o$lengths, 24)
  expect_length(rle_o$values, 24)
  expect_length(unique(rle_o$lengths), 1)
})

# Extracted values (repr.) testing
test_that("Test on x_ts, extracted values from repr_fea...() functions", {
  expect_equal(repr_feaclip(x_ts)["max_1"], c(max_1 = 4))
  expect_equal(repr_feaclip(x_ts)["max_0"], c(max_0 = 4))
  expect_equal(repr_feaclip(x_ts)["sum_1"], c(sum_1 = 48))
  expect_equal(mean(repr_featrend(x_ts, func = max, pieces = pieces)), 4)
  expect_equal(repr_feacliptrend(x_ts, func = max, pieces = pieces)[1], 4)
})
