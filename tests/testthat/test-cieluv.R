# standard whitepoint
d65 <- whitepoints_cie1931("D65")
hex <- "#663399"

# point chosen to be approx 50, 50, 50 in xyz100
mat <- matrix(c(76.0693, 12.5457, 5.2885), ncol = 3)
luv_test <-
  pth_new_cieluv(
    mat,
    whitepoint = d65
  )

test_that("pth_new_cieluv works", {
  expect_s3_class(luv_test, c("pth_cieluv", "pth_mat"))
  expect_true(is.matrix(luv_test))
  expect_identical(ncol(luv_test), 3L)
  expect_identical(attr(luv_test, "whitepoint"), d65)
  expect_identical(dimnames(luv_test), list(NULL, c("L*", "u*", "v*")))
})

test_that("transformer works", {

  luv_test_hex <- pth_to_cieluv(hex)
  transformer <- pth_transformer(luv_test_hex)

  expect_identical(
    luv_test_hex,
    transformer(hex)
  )
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
    luv_test,
    ignore_attr = TRUE
  )
})

test_that("`[.pth_to_cieluv`() works", {

  expect_identical(
    luv_test,
    luv_test[1, ],
    ignore_attr = TRUE
  )

  expect_equal(
    luv_test[ , 2:3],
    mat[ , 2:3, drop = FALSE],
    ignore_attr = TRUE
  )

})

test_that("get same result as farver", {

  ref <- "#663399"

  expect_equal(
    pth_to_cieluv(ref, d65),
    farver::decode_colour(ref, to = "luv", white = "D65"),
    ignore_attr = TRUE,
    tolerance = 0.01
  )
})

test_that("make sure origin translates", {

  luv_origin <- pth_new_cieluv(matrix(c(0, 0, 0), ncol = 3))

  expect_identical(
    pth_to_cieluv("#000000"),
    luv_origin,
    ignore_attr = TRUE
  )

  complex <- c("#663399", "#000000", "#FFFFFF")

  expect_equal(
    pth_to_cieluv(complex, d65),
    farver::decode_colour(complex, to = "luv", white = "D65"),
    ignore_attr = TRUE,
    tolerance = 0.01
  )

})
