sfc_blue <- pth_new_surface(c("#42B4E6", "#0087CD"))
sfc_orange <- pth_new_surface(c("#E47F00", "#702407"))

traj_chroma <- c(0, 80, 20)
traj_lum <- c(80, 50, 20)

traj <- pth_new_trajectory(chroma = traj_chroma, lum = traj_lum)

pal_blue <- pth_new_palette_path(traj, sfc_blue)
pal_orange <- pth_new_palette_path(traj, sfc_orange)

pal_div <- pth_palette_join(pal_blue, pal_orange)

step <- 0.5

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

test_that("layers work", {

  expect_snapshot_plot(
    "palette_div",
    pth_plot_surface(pal_div, step = step) +
      pth_layer_control_points(pal_div) +
      pth_layer_palette(pal_div)
  )
})
