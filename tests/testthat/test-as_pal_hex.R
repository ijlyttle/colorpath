test_that("as_pal_hex works", {

  pal_blues <- palette_bezier(mat_luv_blues)

  expect_error(
    as_pal_hex("foo"),
    "does not inherit from class cpath_palette_luv"
  )

  pal_blues_hex <- as_pal_hex(pal_blues)
  expect_type(pal_blues_hex, "closure")

  x <- seq(0, 1, by = 0.1)
  expect_identical(
    pal_blues_hex(x),
    farver::encode_colour(pal_blues(x), from = "luv")
  )

})
