library("ggplot2")

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

mat_gamut <- pth_mat_gamut(n_point = 5)

test_that("pth_plot_polar() works", {

  expect_snapshot_plot(
    "gamut-none",
    pth_plot_polar(mat_gamut)
  )

  expect_snapshot_plot(
    "gamut-full",
    pth_plot_polar(mat_gamut, cvd = pth_cvd_grid_full()) +
      facet_grid(rows = vars(condition), cols = vars(severity))
  )

  hex <- c("#996633", "#663399", "#336699")

  expect_s3_class(
    pth_plot_polar(hex),
    c("gg", "ggplot")
  )

  expect_s3_class(
    pth_plot_polar(pth_to_cieluv(hex)),
    c("gg", "ggplot")
  )

  expect_snapshot_plot(
    "palette-none",
    pth_new_palette_hex(hex) %>% pth_plot_polar()
  )

})
