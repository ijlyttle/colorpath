library("tibble")

test_that("as_mat_luv() works", {

  expect_equal(as_mat_luv(df_hcl_blues), mat_luv_blues, tolerance = 1.e-4)

})
