
pal_blues <- palette_bezier(mat_luv_blues)

test_that("rescaler_x works", {

  # need the "\\[" notation for regular-expression escape
  expect_error(
    rescaler_x(c(-1, 0)),
    "range\\[1\\] not greater than"
  )

  expect_error(
    rescaler_x(c(2, 0)),
    "range\\[1\\] not less than"
  )

  expect_error(
    rescaler_x(c(0, -1)),
    "range\\[2\\] not greater than"
  )

  expect_error(
    rescaler_x(c(0, 2)),
    "range\\[2\\] not less than"
  )

  rlin <- rescaler_x(c(0.25, 0.75))

  expect_s3_class(rlin, "cpath_rescaler")

  expect_identical(
    rlin(c(0, 0.5, 1)),
    c(0.25, 0.5, 0.75)
  )

  rlin_rev <- rescaler_x(c(0.75, 0.25))

  expect_identical(
    rlin_rev(c(0, 0.5, 1)),
    c(0.75, 0.5, 0.25)
  )
})

test_that("f_root_luminance works", {

  froot <- f_root_luminance(55, pal_blues)

  expect_type(froot, "closure")
  expect_equal(
    froot(c(0, 0.5, 1)),
    c(30, 0, -30)
  )

})


test_that("root_luminance works", {

  expect_equal(
    root_luminance(c(25, 55, 85), palette = pal_blues),
    c(0, 0.5, 1)
  )

})

test_that("rescaler_luminance works", {

  rlum <- rescaler_luminance(c(25, 55), pal_blues)
  rlum2 <- rescaler_luminance(c(55, 85), pal_blues)

  expect_s3_class(rlum, "cpath_rescaler")

  expect_identical(rlum(c(0, 1)), c(0, 0.5))
  expect_identical(rlum2(c(0, 1)), c(0.5, 1))

})

test_that("rescaler_bezier works", {

  expect_error(
    rescaler_bezier(mat_luv_blues, "foo"),
    "numeric"
  )

  expect_error(
    rescaler_bezier(mat_luv_blues, -1),
    "not greater than"
  )

  expect_error(
    rescaler_bezier(seq(1, 10)),
    "exactly three columns"
  )

  rbez <- rescaler_bezier(mat_luv_blues)

  expect_s3_class(rbez, "cpath_rescaler")

  # expect symmetry and same start/end points
  expect_identical(
    rbez(c(0, 0.5, 1)),
    c(0, 0.5, 1),
    tolerance = 2.e-4
  )

})
