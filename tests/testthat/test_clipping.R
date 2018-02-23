context("Tests for feature extraction from clipping repr. functions");

# Length of repr_feaclip()
x_ts <- c(rep(1:8, 12))
test_that("Test on x, length of output of repr_feaclip()", {
  expect_equal(length(repr_feaclip(x_ts)), 8);
})
