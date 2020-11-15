test_that("whitepoints_cie1931() works", {

  expect_error(
    whitepoints_cie1931("foo"),
    "not a legal name"
  )

  whitepoint <- whitepoints_cie1931("D65")
  expect_type(whitepoint, "double")
  expect_length(whitepoint, 3L)

})
