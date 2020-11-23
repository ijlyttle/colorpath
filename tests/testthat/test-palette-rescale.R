test_that("pth_palette_rescale_reverse() works", {

  viridis_11 <- hcl.colors(11, "viridis")
  x_11 <- seq(0, 1, length.out = 11)

  pal_viridis <- pth_new_palette_hex(viridis_11)

  pal_viridis_reverse <- pth_palette_rescale_reverse(pal_viridis)

  expect_type(pal_viridis_reverse, "closure")
  expect_s3_class(pal_viridis_reverse, c("pth_palette_hex", "pth_palette"))

  expect_identical(
    pal_viridis_reverse(x_11),
    pal_viridis(1 - x_11)
  )

})
