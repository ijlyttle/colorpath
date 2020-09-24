test_that("rescale_palette() works", {

  pal_blues <- palette_bezier(mat_luv_blues)
  res_reverse <- rescaler_linear_input(c(1, 0))

  expect_error(
    rescale_palette(pal_blues, "foo"),
    "does not inherit from class cpath_rescaler"
  )

  expect_error(
    rescale_palette("foo", res_reverse),
    "does not inherit from class cpath_palette_luv"
  )

  pal_blues_reverse <- rescale_palette(pal_blues, res_reverse)

  expect_s3_class(pal_blues_reverse, "cpath_palette_luv")

  x <- seq(0, 1, by = 0.1)

  expect_equal(
    pal_blues(x),
    pal_blues_reverse(rev(x))
  )

})
