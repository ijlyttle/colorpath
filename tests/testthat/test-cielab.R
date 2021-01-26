# standard whitepoint
d65 <- whitepoints_cie1931("D65")
hex <- "#663399"

# point chosen to be approx 50, 50, 50 in xyz100
mat <- matrix(c(76.06926, 6.777026, 4.439858), ncol = 3)

lab_test <-
  pth_new_cielab(
    mat,
    whitepoint = d65
  )

test_that("pth_new_cielab works", {
  expect_s3_class(lab_test, c("pth_cielab", "pth_mat"))
  expect_true(is.matrix(lab_test))
  expect_identical(ncol(lab_test), 3L)
  expect_identical(attr(lab_test, "whitepoint"), d65)
  expect_identical(dimnames(lab_test), list(NULL, c("L*", "a*", "b*")))
})

test_that("transformer works", {

  lab_test_hex <- pth_to_cielab(hex)
  transformer <- pth_transformer(lab_test_hex)

  expect_identical(
    lab_test_hex,
    transformer(hex)
  )
})

test_that("to_xyz100 works", {
  expect_equal(
    to_xyz100(lab_test),
    matrix(c(50, 50, 50), ncol = 3),
    tolerance = 2.e-4,
    ignore_attr = TRUE
  )
})

test_that("pth_to_cielab works", {
  expect_equal(
    pth_to_cielab(lab_test),
    lab_test,
    ignore_attr = TRUE
  )
})

test_that("`[.pth_to_cielab`() works", {

  expect_identical(
    lab_test,
    lab_test[1, ],
    ignore_attr = TRUE
  )

  expect_equal(
    lab_test[ , 2:3],
    mat[ , 2:3, drop = FALSE],
    ignore_attr = TRUE
  )

})

test_that("get same result as farver", {

  ref <- "#663399"

  expect_equal(
    pth_to_cielab(ref, d65),
    farver::decode_colour(ref, to = "lab", white = "D65"),
    tolerance = 0.02,
    ignore_attr = TRUE
  )
})
