test_that("pth_cvd_grid() works", {

  # input
  expect_error(pth_cvd_grid(cvd = "foo"), "should be one of")
  expect_error(pth_cvd_grid(cvd = "none", severity = -1), "severity >= 0")
  expect_error(pth_cvd_grid(cvd = "none", severity = 2), "severity <= 1")

  # output
  cvd <- c("none", "deutan")
  severity <- c(0, 0.5, 1)

  cvd_grid <- pth_cvd_grid(cvd = cvd, severity = severity)

  expect_s3_class(cvd_grid, "tbl_df")
  expect_named(cvd_grid, c("cvd", "severity"))
  expect_identical(nrow(cvd_grid), length(cvd) * length(severity))
  expect_identical(unique(cvd_grid$cvd), factor(cvd, levels = cvd))
  expect_identical(unique(cvd_grid$severity), severity)

  # pth_cvd_grid_full is syntactic sugar

})
