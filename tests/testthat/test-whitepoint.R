test_that("whitepoint_cie1931 works", {

  expect_error(
    whitepoint_cie1931("foo"),
    "not a legal name"
  )

  whitepoint <- whitepoint_cie1931("D65")
  expect_type(whitepoint, "double")
  expect_length(whitepoint, 3L)

})
