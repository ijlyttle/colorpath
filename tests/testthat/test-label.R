test_that("label_cols() works", {

  mat <- matrix(seq(1, 6), ncol = 3)
  mat <- label_cols(mat, c("a", "b", "c"))

  expect_identical(
    dimnames(mat),
    list(NULL, c("a", "b", "c"))
  )
})
