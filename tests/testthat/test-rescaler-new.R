test_that("pth_rescaler_reverse() works", {

  r <- pth_rescaler_reverse()
  x <- seq(0, 1, length.out = 10)

  expect_type(r, "closure")
  expect_identical(r(x), 1 - x)

})
