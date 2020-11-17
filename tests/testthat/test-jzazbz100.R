# standard whitepoint
d65 <- whitepoints_cie1931("D65")

# point chosen to be approx 50, 50, 50 in xyz100
mat <- matrix(c(75.47733, 5.471956, 4.499812), ncol = 3)
jab_test <-
  pth_new_jzazbz100(
    mat,
    whitepoint = d65
  )

test_that("pth_new_jzazbz100 works", {
  expect_s3_class(jab_test, c("pth_jzazbz100", "pth_mat"))
  expect_true(is.matrix(jab_test))
  expect_identical(ncol(jab_test), 3L)
  expect_identical(attr(jab_test, "whitepoint"), d65)
  expect_identical(dimnames(jab_test), list(NULL, c("J_z", "a_z", "b_z")))
})

test_that("to_xyz100 works", {
  expect_equal(
    to_xyz100(jab_test),
    matrix(c(50, 50, 50), ncol = 3),
    tolerance = 2.e-4,
    ignore_attr = TRUE
  )
})

test_that("pth_to_jzazbz100 works", {
  expect_equal(
    pth_to_jzazbz100(jab_test),
    jab_test
  )
})

test_that("`[.pth_to_jzazbz100`() works", {

  expect_identical(
    jab_test,
    jab_test[1, ]
  )

  expect_equal(
    jab_test[ , 2:3],
    mat[ , 2:3, drop = FALSE],
    ignore_attr = TRUE
  )
})
