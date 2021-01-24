test_that("srgb_gamut works", {

  # input validation
  expect_error(srgb_gamut(-1), "n_point not greater than")
  expect_error(srgb_gamut("a"), "n_point is not a number")
  expect_error(srgb_gamut(1, set = "foo"), "'arg' should be one of")

  # output
  three_cube <- srgb_gamut(3, set = "vertex")

  # we get the right names
  expect_named(three_cube, c("r", "g", "b"))

  # vertices are vertices
  expect_identical(
    three_cube %>% t() %>% as.vector() %>% unique(),
    c(0, 255)
  )

  # each of the sets works - check the number of rows
  expect_identical(srgb_gamut(3, set = "vertex") %>% nrow(), 8L)
  expect_identical(srgb_gamut(3, set = "edge") %>% nrow(), 20L)
  expect_identical(srgb_gamut(3, set = "surface") %>% nrow(), 26L)
  expect_identical(srgb_gamut(3, set = "all") %>% nrow(), 27L)

})
