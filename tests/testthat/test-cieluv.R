# standard whitepoint
d65 <- whitepoints_cie1931("D65")

# point chosen to be approx 50, 50, 50 in xyz100
luv_test <-
  pth_new_cieluv(
    matrix(c(76.0693, 12.5457, 5.2885), ncol = 3),
    whitepoint = d65
  )

test_that("pth_new_cieluv works", {
  expect_s3_class(luv_test, c("pth_cieluv", "pth_mat"))
  expect_true(is.matrix(luv_test))
  expect_identical(ncol(luv_test), 3L)
  expect_identical(attr(luv_test, "whitepoint"), d65)
  expect_identical(dimnames(luv_test), list(NULL, c("L*", "u*", "v*")))
})

test_that("to_xyz100 works", {
  expect_equal(
    to_xyz100(luv_test),
    matrix(c(50, 50, 50), ncol = 3),
    tolerance = 2.e-4,
    ignore_attr = TRUE
  )
})

test_that("pth_to_cieluv works", {
  expect_equal(
    pth_to_cieluv(luv_test),
    luv_test
  )
})

test_that("get same result as farver", {

  ref <- "#663399"

  expect_equal(
    pth_to_cieluv(ref, d65),
    farver::decode_colour(ref, to = "luv", white = "D65"),
    ignore_attr = TRUE
  )
})
