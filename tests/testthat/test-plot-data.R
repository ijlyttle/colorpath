sfc_blue <- pth_new_hue_surface(250)
sfc_orange <- pth_new_hue_surface(35)

traj_chroma <- c(0, 80, 20)
traj_lum <- c(80, 50, 20)

traj <- pth_new_chroma_trajectory(chroma = traj_chroma, lum = traj_lum)

pal_blue <- pth_new_palette_path(traj, sfc_blue)
pal_orange <- pth_new_palette_path(traj, sfc_orange)

pal_div <- pth_palette_join(pal_blue, pal_orange)

test_that("pth_data_surface_raster() works", {

  step <- 20

  expect_error(
    pth_data_surface_raster("foo"),
    "No method"
  )

  df_sfc_blue <- pth_data_surface_raster(sfc_blue, step = step)

  # check class, column-names
  expect_s3_class(df_sfc_blue, "data.frame")
  expect_named(
    df_sfc_blue,
    c("luminance", "chroma", "hue", "hex")
  )

  # check contents
  expect_identical(
    pth_data_surface_raster(pal_blue, step = step),
    df_sfc_blue
  )

  # check diverging
  df_sfc_orange <- pth_data_surface_raster(sfc_orange, step = step)
  df_sfc_blue_reverse <- df_sfc_blue
  df_sfc_blue_reverse$chroma <- -df_sfc_blue_reverse$chroma

  expect_identical(
    pth_data_surface_raster(pal_div, step = step),
    rbind(df_sfc_blue_reverse, df_sfc_orange)
  )

})

test_that("pth_data_control_points() works", {

  expect_error(
    pth_data_control_points("foo"),
    "No method"
  )

  df_traj <- pth_data_control_points(traj)

  # check class, column-names
  expect_s3_class(df_traj, "data.frame")
  expect_named(
    df_traj,
    c("luminance", "chroma")
  )

  # check contents
  expect_identical(df_traj$luminance, traj_lum)
  expect_identical(df_traj$chroma, traj_chroma)

  # negate chroma, reverse row-order
  df_traj_rev <- df_traj
  df_traj_rev$chroma <- -df_traj_rev$chroma
  df_traj_rev <- df_traj_rev[rev(seq_len(nrow(df_traj_rev))), ]

  expect_identical(
    pth_data_control_points(pal_div),
    rbind(df_traj_rev, df_traj)
  )
})
