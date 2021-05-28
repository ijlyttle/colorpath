# standard conditions
d65 <- whitepoints_cie1931("D65")
c <- 0.69
Y_b <- 20
L_A <- 64 / pi / 5
exact_inversion <- TRUE

hex <- "#663399"

# point chosen to be approx 50, 50, 50 in xyz100
mat <- matrix(c(100, 0, 0), ncol = 3)
cam16_test <-
  pth_new_cam16ucs(
    mat,
    c = c,
    Y_b = Y_b,
    L_A = L_A,
    exact_inversion = exact_inversion,
    whitepoint = d65
  )

test_that("pth_new_cam16ucs() works", {
  expect_s3_class(cam16_test, c("pth_cam16ucs", "pth_mat"))
  expect_true(is.matrix(cam16_test))
  expect_identical(ncol(cam16_test), 3L)
  expect_identical(attr(cam16_test, "c"), c)
  expect_identical(attr(cam16_test, "Y_b"), Y_b)
  expect_identical(attr(cam16_test, "L_A"), L_A)
  expect_identical(attr(cam16_test, "exact_inversion"), exact_inversion)
  expect_identical(attr(cam16_test, "whitepoint"), d65)
  expect_identical(dimnames(cam16_test), list(NULL, c("J'", "a'", "b'")))
})

test_that("transformer works", {

  cam16_test_hex <- pth_to_cam16ucs(hex)
  transformer <- pth_transformer(cam16_test_hex)

  expect_identical(
    cam16_test_hex,
    transformer(hex)
  )
})

test_that("creator works", {

  creator <- pth_creator(cam16_test)

  expect_identical(
    cam16_test,
    creator(mat)
  )
})

test_that("to_xyz100() works", {
  expect_equal(
    to_xyz100(cam16_test),
    matrix(c(95.70465, 99.83928, 107.1163), ncol = 3),
    tolerance = 1.e-4,
    ignore_attr = TRUE
  )
})

test_that("pth_to_cam16ucs() works", {
  expect_equal(
    pth_to_cam16ucs(cam16_test),
    cam16_test
  )
})

test_that("`[.pth_to_cam16ucs`() works", {
  expect_identical(
    cam16_test,
    cam16_test[1, ]
  )

  expect_equal(
    cam16_test[ , 2:3],
    mat[ , 2:3, drop = FALSE],
    ignore_attr = TRUE
  )
})
