test_that("rescaler_linear works", {

  expect_error(
    rescaler_linear(-1, 0),
    "x0 not greater than"
  )

  expect_error(
    rescaler_linear(2, 0),
    "x0 not less than"
  )

  expect_error(
    rescaler_linear(0, -1),
    "x1 not greater than"
  )

  expect_error(
    rescaler_linear(0, 2),
    "x1 not less than"
  )

  rlin <- rescaler_linear(0.25, 0.75)

  expect_s3_class(rlin, "cpath_rescaler")

  expect_identical(
    rlin(c(0, 0.5, 1)),
    c(0.25, 0.5, 0.75)
  )

  rlin_rev <- rescaler_linear(0.75, 0.25)

  expect_identical(
    rlin_rev(c(0, 0.5, 1)),
    c(0.75, 0.5, 0.25)
  )
})

test_that("rescaler_bezier works", {

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

  rbez <- rescaler_bezier(mat_luv)

  expect_s3_class(rbez, "cpath_rescaler")

  # expect symmetry and same start/end points
  expect_identical(
    rbez(c(0, 0.5, 1)),
    c(0, 0.5, 1),
    tolerance = 1.5e-4
  )

})
