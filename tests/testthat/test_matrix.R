context("Tests for repr_matrix() and repr_list() functions");

# dimension of a matrix
data("elec_load")
win_size <- 48
test_that("Test on elec_load, dimension of output from repr_matrix() function", {
  expect_equal(dim(repr_matrix(elec_load, func = repr_feaclip, windowing = TRUE, win_size = win_size)),
                c(nrow(elec_load), 8*ncol(elec_load)/win_size))
  expect_equal(dim(repr_matrix(elec_load, func = repr_feaclip)),
               c(nrow(elec_load), 8))
  expect_equal(dim(repr_matrix(elec_load, func = repr_feaclip, normalise = T, func_norm = norm_z)),
               c(nrow(elec_load), 8))
})

# Test errors
test_that("Test on elec_load, errors on repr_matrix() function", {
  expect_error(repr_matrix(elec_load, func = repr_feaclip, windowing = TRUE), "win_size must be specified!")
  expect_error(repr_matrix(elec_load), "func must be specified!")
})
