sfc_blue <- pth_new_hue_surface(250)
sfc_orange <- pth_new_hue_surface(35)

traj_chroma <- c(0, 80, 20)
traj_lum <- c(80, 50, 20)

traj <- pth_new_chroma_trajectory(chroma = traj_chroma, lum = traj_lum)

pal_blue <- pth_new_palette_path(traj, sfc_blue)
pal_orange <- pth_new_palette_path(traj, sfc_orange)

pal_div <- pth_palette_join(pal_blue, pal_orange)

expect_snapshot_plot <- function(name, code) {
  # Other packages might affect results
  skip_if_not_installed("ggplot2", "2.0.0")

  # don't run on CI
  skip_on_ci()

  filename <- tempfile(fileext = ".png")

  suppressMessages(
    ggplot2::ggsave(filename = filename, plot = code)
  )

  expect_snapshot_file(filename, paste0(name, ".png"))
}


test_that("pth_plot_surface() works", {

  step <- 20

  expect_error(
    pth_plot_surface("foo"),
    "No method"
  )

  expect_snapshot_plot(
    "surface_blue",
    pth_plot_surface(pal_blue, step = step)
  )

  expect_snapshot_plot(
    "surface_div",
    pth_plot_surface(pal_div, step = step)
  )

})
