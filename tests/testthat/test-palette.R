library("magrittr")

test_that("pth_new_palette_path() works", {

  sfc_blues_single <- pth_new_surface("#0000FF")

  lum <- c(20, 50, 80)
  chroma <- c(0, 100, 0)
  traj <- pth_new_trajectory(lum, chroma)

  # validate inputs
  expect_error(
    pth_new_palette_path("foo", sfc_blues_single),
    "trajectory"
  )

  expect_error(
    pth_new_palette_path(traj, "foo"),
    "surface"
  )

  expect_error(
    pth_new_palette_path(traj, sfc_blues_single, constructor = "foo"),
    "constructor"
  )

  # test palettes
  palette <-
    pth_new_palette_path(traj, sfc_blues_single)

  expect_type(palette, "closure")
  expect_s3_class(palette, c("pth_palette_path", "pth_palette"))

  luv <- palette(c(0, 0.5, 1))
  expect_s3_class(luv, c("pth_cieluv", "pth_mat"))

})

test_that("pth_new_palette_hex() works", {

  viridis_11 <-
    grDevices::hcl.colors(11, palette = "viridis") %>%
    pth_to_hex()

  viridis_21 <-
    grDevices::hcl.colors(21, palette = "viridis") %>%
    pth_to_hex()

  expect_error(
    pth_new_palette_hex("foo"),
    "hex-code"
  )

  expect_error(
    pth_new_palette_hex(viridis_11[[1]]),
    "not greater than"
  )

  expect_error(
    pth_new_palette_hex(viridis_11, identity),
    "pth_mat"
  )

  pal_viridis <- pth_new_palette_hex(viridis_11, transformer = pth_to_cieluv)

  x_11 <- seq(0, 1, length.out = 11)
  x_21 <- seq(0, 1, length.out = 21)

  # expect nodes identical
  expect_identical(
    pal_viridis(x_11) %>% pth_to_hex(),
    viridis_11
  )

  expect_identical(
    pal_viridis(x_21) %>% pth_to_hex() %>% `[`(x_21 %in% x_11),
    viridis_11
  )

  # expect interpolation distances to be small
  expect_lt(
    pth_distance_euclid(
      viridis_21,
      pal_viridis(x_21),
      transformer = pth_to_cieluv
    ) %>%
    max(),
    2.06
  )

})
