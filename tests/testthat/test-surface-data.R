test_that("pth_surface_data works", {

  sfc <- pth_new_surface("#0000FF", transformer = pth_to_cieluv)

  expect_snapshot(pth_surface_data(sfc, step = 10))

})
