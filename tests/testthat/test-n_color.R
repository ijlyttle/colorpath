test_that("pth_n_color() works", {
  expect_error(
    pth_n_color(3),
    "No method"
  )

  hex <- c("#000000", "#663399", "#FFFFFF")
  expect_identical(pth_n_color(hex), 3L)

  luv <- pth_to_cieluv(hex)
  expect_identical(pth_n_color(luv), 3L)
})
