test_that("pal_luv_bezier() works", {

  expect_error(
    pal_luv_bezier("foo"),
    "not a matrix"
  )

  # out endpoints and midpoint
  #  - out endpoints will be identical to control endpoints
  #  - out midpoint will have half U,V as the control midpoints
  mat_luv_out <- mat_luv_blues
  mat_luv_out[2, 2:3] <- mat_luv_out[2, 2:3] / 2

  pb <- pal_luv_bezier(mat_luv_blues, rescale_path = FALSE)

  expect_s3_class(pb, "cpath_pal_luv")

  expect_equal(
    pb(c(0, 0.5, 1)),
    mat_luv_out,
    tolerance = 1.e-4
  )

  pb_rescaled <- pal_luv_bezier(mat_luv_blues, rescale_path = TRUE)

  expect_s3_class(pb_rescaled, "cpath_pal_luv")
})
