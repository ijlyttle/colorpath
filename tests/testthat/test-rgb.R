hex_ref <- c("#000000", "#663399", "#ffffff")
rgb_ref <-
  matrix(
    c(0, 0, 0, 102, 51, 153, 255, 255, 255),
    ncol = 3,
    byrow = TRUE
  )
dimnames(rgb_ref) <- list(NULL, c("r", "g", "b"))


test_that("internal functions work", {

  # oog == out of gamut
  rgb_oog_ref <-
    matrix(
      c(-1, 0, 0, 102, -1, 269, 255, 255, 258),
      ncol = 3,
      byrow = TRUE
    )
  dimnames(rgb_oog_ref) <- list(NULL, c("r", "g", "b"))

  # we get the same result back
  expect_equal(
    xyz100_to_rgb255(rgb255_to_xyz100(rgb_oog_ref)),
    rgb_oog_ref
  )

})

test_that("to_rgb() errors correctly", {
  expect_error(to_rgb(2), "No method for class numeric")
})

test_that("to_rgb() works", {

  expect_equal(to_rgb(hex_ref), rgb_ref)

  expect_equal(to_rgb(pth_to_cielab(hex_ref)), rgb_ref, tolerance = 1.e-4)

})
