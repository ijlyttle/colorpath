# standard conditions
d65 <- whitepoints_cie1931("D65")
c <- 0.69
Y_b <- 20
L_A <- 64 / pi / 5

# point chosen to be approx 50, 50, 50 in xyz100
mat <- matrix(c(100, 0, 0), ncol = 3)
cam02_test <-
  pth_new_cam02ucs(
    mat,
    c = c,
    Y_b = Y_b,
    L_A = L_A,
    whitepoint = d65
  )

test_that("pth_new_cam02ucs() works", {
  expect_s3_class(cam02_test, c("pth_cam02ucs", "pth_mat"))
  expect_true(is.matrix(cam02_test))
  expect_identical(ncol(cam02_test), 3L)
  expect_identical(attr(cam02_test, "c"), c)
  expect_identical(attr(cam02_test, "Y_b"), Y_b)
  expect_identical(attr(cam02_test, "L_A"), L_A)
  expect_identical(attr(cam02_test, "whitepoint"), d65)
  expect_identical(dimnames(cam02_test), list(NULL, c("J'", "a'", "b'")))
})

test_that("to_xyz100() works", {
  expect_equal(
    to_xyz100(cam02_test),
    matrix(c(95.6524, 99.78746, 107.0885), ncol = 3),
    tolerance = 1.e-4,
    ignore_attr = TRUE
  )
})

test_that("pth_to_cam02ucs() works", {
  expect_equal(
    pth_to_cam02ucs(cam02_test),
    cam02_test
  )
})

test_that("`[.pth_to_cam02ucs`() works", {

  expect_identical(
    cam02_test,
    cam02_test[1, ]
  )

  expect_equal(
    cam02_test[ , 2:3],
    mat[ , 2:3, drop = FALSE],
    ignore_attr = TRUE
  )

})
