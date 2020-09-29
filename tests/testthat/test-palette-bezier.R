test_that("palette_bezier() works", {

  expect_error(
    palette_bezier("foo"),
    "not a matrix"
  )

  # out endpoints and midpoint
  #  - out endpoints will be identical to control endpoints
  #  - out midpoint will have half U,V as the control midpoints
  mat_luv_out <- mat_luv_blues
  mat_luv_out[2, 2:3] <- mat_luv_out[2, 2:3] / 2

  pb <- palette_bezier(mat_luv_blues)

  expect_s3_class(pb, "cpath_pal_luv")

  expect_identical(
    pb(c(0, 0.5, 1)),
    mat_luv_out
  )

})
