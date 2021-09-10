chr <- c("#00FFFF", "#0000FF")
hex <- pth_to_hex(chr)
mat <- pth_to_cieluv(hex)
data <- pth_data_cvd(mat, cvd = pth_cvd_grid_deupro())

plot <- pth_plot_lumchroma(data, name_color_space = "CIELUV")

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

test_that("pth_plot_lumchroma() works", {

  expect_snapshot_plot(
    "character",
    pth_plot_lumchroma(chr, transformer = pth_to_cieluv)
  )

  expect_snapshot_plot(
    "hex",
    pth_plot_lumchroma(hex, transformer = pth_to_cieluv)
  )

  expect_snapshot_plot(
    "pth_mat",
    pth_plot_lumchroma(mat)
  )

  expect_snapshot_plot(
    "data-frame",
    pth_plot_lumchroma(data) # should say color-space unknown
  )
})
