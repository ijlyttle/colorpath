test_that("is_mat() works", {

  expect_true(
    is_mat(matrix(1, nrow = 2, ncol = 3))
  )

  # not numeric
  expect_false(
    is_mat(matrix("1", nrow = 2, ncol = 3))
  )

  # not matrix
  expect_false(
    is_mat(rep(1, 6))
  )

  # not three columns
  expect_false(
    is_mat(matrix(1, nrow = 2, ncol = 2))
  )

})

polar_nonzero <- matrix(c(60, 25, 60), ncol = 3)
cart_nonzero <- matrix(c(60, 12.5, 12.5 * sqrt(3)), ncol = 3)

test_that("pth_to_cartesian() works", {

  polar_zero <- matrix(c(50, 0, 90), ncol = 3)
  cart_zero <-  matrix(c(50, 0, 1.e-2), ncol = 3)

  expect_equal(
    pth_to_cartesian(polar_zero, chroma_min = 1.e-2),
    cart_zero
  )

  expect_equal(
    pth_to_cartesian(polar_nonzero),
    cart_nonzero
  )
})

test_that("pth_to_polar() works", {
  expect_equal(
    pth_to_polar(cart_nonzero),
    polar_nonzero
  )
})


test_that("rbind() works", {

  blue_luv <- pth_to_cieluv("#0000FF")
  blue_lab <- pth_to_cielab("#0000FF")

  # mismatched color spaces
  expect_error(rbind(blue_luv, blue_lab))

  blues <- rbind(blue_luv, blue_luv)

  expect_s3_class(blues, class(blue_luv))
  expect_identical(dim(blues), c(2L, 3L))
  expect_equal(blues[1, ], blue_luv)
  expect_equal(blues[2, ], blue_luv)

})
