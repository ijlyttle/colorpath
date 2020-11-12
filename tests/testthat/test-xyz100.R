test_that("default for xyz100 functions work", {
  expect_error(to_xyz100(4), "No method for class")
  expect_error(from_xyz100(4), "No method for class")
})
