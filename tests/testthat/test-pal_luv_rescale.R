test_that("pal_luv_rescale() works", {

  pal_blues <- pal_luv_bezier(mat_luv_blues)
  res_reverse <- rescaler_x(c(1, 0))

  expect_error(
    pal_luv_rescale(pal_blues, "foo"),
    "does not inherit from class cpath_rescaler"
  )

  expect_error(
    pal_luv_rescale("foo", res_reverse),
    "does not inherit from class cpath_pal_luv"
  )

  pal_blues_reverse <- pal_luv_rescale(pal_blues, res_reverse)

  expect_s3_class(pal_blues_reverse, "cpath_pal_luv")

  x <- seq(0, 1, by = 0.1)

  expect_equal(
    pal_blues(x),
    pal_blues_reverse(rev(x))
  )

})
