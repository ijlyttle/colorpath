test_that("pth_mat_replace_data() works", {

  luv_test <- pth_to_cieluv("#663399")
  luv_new <- pth_to_cieluv(c("#000000", "#663399", "#FFFFFF"))

  expect_error(
    pth_mat_replace_data(luv_test, 3),
    "is_mat"
  )

  expect_error(
    pth_mat_replace_data(3, luv_new),
    "No method"
  )

  expect_identical(
    pth_mat_replace_data(luv_test, luv_new),
    luv_new,
    ignore_attr = TRUE
  )

})
