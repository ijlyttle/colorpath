test_that("colorio works", {
  expect_s3_class(
    colorio,
    c("python.builtin.module", "python.builtin.object"),
    exact = TRUE
  )
})
