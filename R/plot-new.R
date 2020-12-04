#' Plot hue-surfaces
#'
#' The calculation is a somewhat slow - at the "root" of the speed problem
#' is the calculation of the maximum chroma. In the future, we could imagine
#' providing a lookup table, as colorspace does, to speed things up.
#'
#' @param x `function` with S3 class `pth_palette_path` or `pth_hue_surface`,
#'  or `data.frame` with columns `luminance`, `chroma`, `hue`, `hex`
#' @inheritParams pth_new_palette_path
#' @inheritParams pth_data_surface_raster
#' @param ... other arguments passed on to `constructor`
#'
#' @return `tibble` with columns `luminance`, `chroma`, `hue`, `hex`
#'
#' @export
#'
pth_plot_surface <- function(x, ...) {
  UseMethod("pth_plot_surface")
}

#' @rdname pth_plot_surface
#' @export
#'
pth_plot_surface.default <- function(x, ...) {
  stop(
    glue::glue("No method for class {class(x)}")
  )
}

#' @rdname pth_plot_surface
#' @export
#'
pth_plot_surface.data.frame <- function(x, ...) {
  plot_surface(x)
}
#
#' @rdname pth_plot_surface
#' @export
#'
pth_plot_surface.pth_hue_surface <- function(x, step = 0.5,
                                             constructor = pth_new_cieluv,
                                             ...) {
  data <- pth_data_surface_raster(x, step, constructor, ...)
  plot_surface(data)
}

#' @rdname pth_plot_surface
#' @export
#'
pth_plot_surface.pth_palette_path <- function(x, step = 0.5, ...) {
  data <- pth_data_surface_raster(x, step, ...)
  plot_surface(data)
}

plot_surface <- function(data) {

  g <-
    ggplot2::ggplot(
      data,
      ggplot2::aes_string(x = "chroma", y = "luminance", fill = "hex")
    ) +
    ggplot2::geom_raster() +
    ggplot2::scale_fill_identity() +
    ggplot2::labs(
      x = "chroma",
      y = "luminance"
    ) +
    ggplot2::coord_fixed(ratio = 1) +
    ggplot2::theme_bw()

  g
}
