pal_blues <- pal_luv_bezier(mat_luv_blues)
x <- seq(0, 1, by = 0.1)

test_that("pal_luv_rescale() works", {

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

  expect_equal(
    pal_blues(x),
    pal_blues_reverse(rev(x))
  )

})

test_that("pal_luv_rescale_x() works", {

  range_x <- c(0.25, 0.75)
  rescaler_x <- rescaler_x(range_x)
  pal_x1 <- pal_luv_rescale(pal_blues, rescaler_x)
  pal_x2 <- pal_luv_rescale_x(pal_blues, range_x)

  expect_identical(pal_x1(x), pal_x2(x))

})

test_that("pal_luv_rescale_lum() works", {

  range_lum <- c(40, 80)
  rescaler_lum <- rescaler_lum(range_lum, pal_blues)
  pal_lum1 <- pal_luv_rescale(pal_blues, rescaler_lum)
  pal_lum2 <- pal_luv_rescale_lum(pal_blues, range_lum)

  expect_identical(pal_lum1(x), pal_lum2(x))

})
