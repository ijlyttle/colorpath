#' Polar plot
#'
#' @param data object coercible to `data.frame` with columns
#'   `luminance`, `chroma`, `hue`, `hex`.
#' @inheritParams pth_data_cvd
#'
#' @return Object with S3 classes `"gg"`, `"ggplot"`; i.e. a ggplot
#' @export
#'
pth_plot_polar <- function(data, ...) {
  UseMethod("pth_plot_polar")
}

#' @rdname pth_plot_polar
#' @export
#'
pth_plot_polar.default <- function(data, ...) {
  stop(
    glue::glue("No method for class {class(data)}")
  )
}


#' @rdname pth_plot_polar
#' @export
#'
pth_plot_polar.data.frame <- function(data, ...) {
  plot_polar(data)
}

#' @rdname pth_plot_polar
#' @export
#'
pth_plot_polar.character <- function(data, cvd = pth_cvd_grid_none(),
                                     transformer = pth_to_cieluv, ...) {

  data <- pth_data_cvd(data, cvd = cvd, transformer = transformer)

  pth_plot_polar(data)
}

#' @rdname pth_plot_polar
#' @export
#'
pth_plot_polar.pth_mat <- function(data, cvd = pth_cvd_grid_none(), ...) {

  data <- pth_data_cvd(data, cvd = cvd)

  pth_plot_polar(data)
}

#' @rdname pth_plot_polar
#' @export
#'
pth_plot_polar.pth_palette <- function(data, cvd = pth_cvd_grid_none(),
                                   n_point = 11, ...) {

  data <- pth_data_cvd(data, cvd = cvd, n_point = n_point)

  pth_plot_polar(data)
}

plot_polar <- function(data = NULL) {

  plot <-
    ggplot2::ggplot(
      data = data,
      mapping = ggplot2::aes_string(x = "hue", y = "chroma", color = "hex")
    ) +
    ggplot2::geom_point() +
    ggplot2::scale_x_continuous(limits = c(0, 360), breaks = seq(0, 360, 60)) +
    ggplot2::scale_y_continuous(limits = c(0, NA)) +
    ggplot2::scale_color_identity() +
    ggplot2::coord_polar(theta = "x", start = -pi / 2, direction = -1) +
    ggplot2::theme_bw()

  plot
}
