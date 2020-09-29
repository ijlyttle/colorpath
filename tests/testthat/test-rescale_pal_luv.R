test_that("rescale_pal_luv() works", {

  pal_blues <- pal_luv_bezier(mat_luv_blues)
  res_reverse <- rescaler_x(c(1, 0))

  expect_error(
    rescale_pal_luv(pal_blues, "foo"),
    "does not inherit from class cpath_rescaler"
  )

  expect_error(
    rescale_pal_luv("foo", res_reverse),
    "does not inherit from class cpath_pal_luv"
  )

  pal_blues_reverse <- rescale_pal_luv(pal_blues, res_reverse)

  expect_s3_class(pal_blues_reverse, "cpath_pal_luv")

  x <- seq(0, 1, by = 0.1)

  expect_equal(
    pal_blues(x),
    pal_blues_reverse(rev(x))
  )

})
