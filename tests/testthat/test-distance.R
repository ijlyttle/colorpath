pal_blues <- pal_luv_bezier(mat_luv_blues, rescale_path = FALSE)

test_that("get_distance() works", {
  expect_error(get_distance("foo"), "cpath_pal_luv")
  expect_error(get_distance(pal_blues, n = "foo"), "numeric")
  expect_error(get_distance(pal_blues, n = 0), "greater than")

  n <- 20
  dist <- get_distance(pal_blues, n = n)

  expect_length(dist, n)
  expect_type(dist, "double")
  expect_true(all(dist > 0))
})
