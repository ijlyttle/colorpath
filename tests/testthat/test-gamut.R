test_that("pth_mat_gamut works", {

  # input validation
  expect_error(pth_mat_gamut(-1), "n_point not greater than")
  expect_error(pth_mat_gamut("a"), "n_point is not a number")
  expect_error(pth_mat_gamut(1, set = "foo"), "'arg' should be one of")

  # output
  three_cube <- pth_mat_gamut(3, set = "vertex", transformer = pth_to_cieluv)

  expect_type(three_cube, "double")
  expect_s3_class(three_cube, c("pth_cieluv", "pth_mat"))
})

test_that("srgb255_gamut works", {

  # input validation
  expect_error(srgb255_gamut(-1), "n_point not greater than")
  expect_error(srgb255_gamut("a"), "n_point is not a number")
  expect_error(srgb255_gamut(1, set = "foo"), "'arg' should be one of")

  # output
  three_cube <- srgb255_gamut(3, set = "vertex")

  expect_type(three_cube, "double")
  expect_identical(dimnames(three_cube), list(NULL, c("r", "g", "b")))

  # vertices are vertices
  expect_identical(
    three_cube %>% t() %>% as.vector() %>% unique(),
    c(0, 255)
  )

  # each of the sets works - check the number of rows
  expect_identical(srgb255_gamut(3, set = "vertex") %>% nrow(), 8L)
  expect_identical(srgb255_gamut(3, set = "edge") %>% nrow(), 20L)
  expect_identical(srgb255_gamut(3, set = "surface") %>% nrow(), 26L)
  expect_identical(srgb255_gamut(3, set = "all") %>% nrow(), 27L)
})
