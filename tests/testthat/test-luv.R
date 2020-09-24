library("tibble")

test_that("luv() works", {

  expect_equal(luv(df_hcl_blues), mat_luv_blues, tolerance = 1.e-4)

})
