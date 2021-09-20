sfc_blue <- pth_new_surface("#0000FF")
sfc_orange <- pth_new_surface("#E5813D")

traj_chroma <- c(0, 80, 20)
traj_lum <- c(80, 50, 20)

traj <- pth_new_trajectory(lum = traj_lum, chroma = traj_chroma)

pal_blue <- pth_new_palette_path(traj, sfc_blue)
pal_orange <- pth_new_palette_path(traj, sfc_orange)

pal_div <- pth_palette_join(pal_blue, pal_orange)

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

test_that("pth_data_palette() works", {

  expect_error(
    pth_data_palette("foo"),
    "No method"
  )

  df_blue <- pth_data_palette(pal_blue)

  # check class, column-names
  expect_s3_class(df_blue, "data.frame")
  expect_named(
    df_blue,
    c("luminance", "chroma", "hue", "hex")
  )

})
