test_that("palette_bezier() works", {

  expect_error(
    palette_bezier("foo"),
    "not a matrix"
  )

  mat_luv <- matrix(
    c(
      20,  50, 80, # l
      0,  -46,  0, # u
      0,   56,  0  # v
    ),
    ncol = 3L,
    byrow = FALSE,
    dimnames = list(NULL, c("l", "u", "v"))
  )

  # out endpoints and midpoint
  #  - out endpoints will be identical to control endpoints
  #  - out midpoint will have half U,V as the control midpoints
  mat_luv_out <- mat_luv
  mat_luv_out[2, 2:3] <- mat_luv_out[2, 2:3] / 2

  pb <- palette_bezier(mat_luv)

  expect_s3_class(pb, "cpath_palette_luv")

  expect_identical(
    pb(c(0, 0.5, 1)),
    mat_luv_out
  )

})
