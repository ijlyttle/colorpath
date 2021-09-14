sfc_blue <- pth_new_surface("#0000FF")

test_that("pth_color_sfclumsat() validates", {
  expect_error(pth_color_sfclumsat(10, lum = 50, sat = 1))
  expect_error(pth_color_sfclumsat(sfc_blue, lum = "a", sat = 1))
  expect_error(pth_color_sfclumsat(sfc_blue, lum = 0, sat = "a"))
  expect_error(pth_color_sfclumsat(sfc_blue, lum = -1, sat = 1))
  expect_error(pth_color_sfclumsat(sfc_blue, lum = 101, sat = 1))
  expect_error(pth_color_sfclumsat(sfc_blue, lum = 0, sat = -1))
  expect_error(pth_color_sfclumsat(sfc_blue, lum = 0, sat = 2))
})

test_that("pth_color_sfclumsat() works", {

  lum <- 50
  sat_full <- 1
  sat_half <- 0.5

  color_full <- pth_color_sfclumsat(sfc_blue, lum = lum, sat = sat_full)
  chroma_full <- pth_to_polar(color_full)[, 2]

  color_half <- pth_color_sfclumsat(sfc_blue, lum = lum, sat = sat_half)
  chroma_half <- pth_to_polar(color_half)[, 2]

  expect_s3_class(color_full, "pth_mat")

  expect_equal(color_full[, 1], lum, tolerance = 1.e-3, ignore_attr = TRUE)
  expect_equal(
    pth_max_chroma(color_full) * sat_full,
    chroma_full,
    tolerance = 1.e-3
  )

  expect_equal(color_half[, 1], lum, tolerance = 1.e-3, ignore_attr = TRUE)
  expect_equal(
    pth_max_chroma(color_half) * sat_half,
    chroma_half,
    tolerance = 1.e-3
  )
})


