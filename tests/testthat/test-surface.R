test_that("pth_new_surface works", {

  sfc_single <- pth_new_surface("#0000FF", transformer = pth_to_cieluv)
  sfc_double <- pth_new_surface(c("#0000FF", "#00FFFF"))
  sfc_mat <- pth_new_surface(pth_to_cieluv("#0000FF"))

  expect_s3_class(sfc_single, "pth_surface")
  expect_s3_class(sfc_double, "pth_surface")
  expect_s3_class(sfc_mat, "pth_surface")

  expect_type(sfc_single$fn_hue, "closure")
  expect_type(sfc_single$fn_max_chroma, "closure")
  expect_s3_class(sfc_single$colors, c("pth_mat", "pth_cieluv"))

})
