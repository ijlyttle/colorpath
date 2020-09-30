pal_blues <- pal_luv_bezier(mat_luv_blues)

test_that("new_pal_luv works", {

  expect_s3_class(pal_blues, "cpath_pal_luv")

  expect_identical(spec_luv(pal_blues), mat_luv_blues)

})
