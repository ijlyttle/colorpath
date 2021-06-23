blue <- pth_to_cieluv("#663399")
sfc_blue <- pth_new_surface(blue, n_step = 2)

test_that("pth_colorspace_name works", {
  expect_identical(pth_colorspace_name(sfc_blue), "CIELUV")
  expect_identical(pth_colorspace_name(blue), "CIELUV")
})
