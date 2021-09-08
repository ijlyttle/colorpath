library("magrittr")

test_that("pth_new_palette_path() works", {

  hue <- 225
  sfc_blues_single <- pth_new_hue_surface(hue)

  chroma <- c(0, 100, 0)
  lum <- c(20, 50, 80)
  traj <- pth_new_chroma_trajectory(chroma, lum)

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
    pth_new_palette_path(traj, sfc_blues_single, constructor = pth_new_cieluv)

  expect_type(palette, "closure")
  expect_s3_class(palette, c("pth_palette_path", "pth_palette"))

  luv <- palette(c(0, 0.5, 1))

  expect_s3_class(luv, c("pth_cieluv", "pth_mat"))

  # placeholder for u,v
  uv <- sqrt(0.5) * chroma / 2
  expect_equal(
    luv,
    matrix(c(20, 50, 80, -uv, -uv), ncol = 3, byrow = FALSE),
    tolerance = 1.e-5,
    ignore_attr = TRUE
  )

  # make sure we have preserved the hue
  sfc_blues_multi <- pth_new_hue_surface(c(225, 270))

  palette_multi <- pth_new_palette_path(traj, sfc_blues_multi)

  cart <- palette_multi(seq(0, 1, by = 0.1))
  polar <- pth_to_polar(cart)

  expect_equal(
    sfc_blues_multi(polar[, 1]), # calculate hue using surface
    polar[, 3] # hue from palette
  )

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
