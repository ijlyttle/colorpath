pal_blues <- pal_luv_bezier(mat_luv_blues)

test_that("as_pal_hex works", {

  expect_error(
    as_pal_hex("foo"),
    "does not inherit from class cpath_pal_luv"
  )

  pal_blues_hex <- as_pal_hex(pal_blues)
  expect_type(pal_blues_hex, "closure")

  x <- seq(0, 1, by = 0.1)
  expect_identical(
    pal_blues_hex(x),
    farver::encode_colour(pal_blues(x), from = "luv")
  )

})

test_that("as_pal_disc works", {

  n <- 6
  x <- seq(0, 1, length.out = n)

  pal_blues_disc <- as_pal_disc(pal_blues)

  expect_type(pal_blues_disc, "closure")

  expect_identical(
    pal_blues(x),
    pal_blues_disc(n)
  )

})
