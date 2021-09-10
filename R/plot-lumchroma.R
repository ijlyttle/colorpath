#' Plot colors with CVD in luminance-chroma plane
#'
#' Oversimplifying things, color-vision deficiency collapses the color-wheel
#' into a "color-line", such that luminance and chroma are the important
#' dimensions.
#'
#' @param x Object coerced into a CVD data-frame (`pth_data_cvd()`). Can be a
#' hex-code, `pth_mat`, or even a data frame.
#' @param name_color_space `character` name of the color space used.
#' @inheritParams pth_layer_palette
#' @inheritParams pth_data_cvd
#'
#' @return Object with S3 class `ggplot`.
#' @export
#'
pth_plot_lumchroma <- function(x, ...) {
  UseMethod("pth_plot_lumchroma")
}

#' @rdname pth_plot_lumchroma
#' @export
#'
pth_plot_lumchroma.default <- function(x, ...) {
  stop(
    glue::glue("No method for class {class(x)}")
  )
}

#' @rdname pth_plot_lumchroma
#' @export
#'
pth_plot_lumchroma.character <- function(x, cvd = pth_cvd_grid_deupro(),
                                         transformer = NULL,
                                         color_point_inner = "white",
                                         color_point_outer = "black",
                                         size_point_inner = 6,
                                         size_point_outer = 7,
                                         ...) {
  .hex <- pth_to_hex(x)

  pth_plot_lumchroma(
    x = .hex,
    cvd = cvd,
    transformer = transformer,
    color_point_inner = color_point_inner,
    color_point_outer = color_point_outer,
    size_point_inner = size_point_inner,
    size_point_outer = size_point_outer
  )
}

#' @rdname pth_plot_lumchroma
#' @export
#'
pth_plot_lumchroma.pth_hex <- function(x, cvd = pth_cvd_grid_deupro(),
                                       transformer = NULL,
                                       color_point_inner = "white",
                                       color_point_outer = "black",
                                       size_point_inner = 6,
                                       size_point_outer = 7,
                                       ...) {

  transformer <- transformer %||% pth_to_cieluv

  .mat <- transformer(x)

  pth_plot_lumchroma(
    x = .mat,
    cvd = cvd,
    color_point_inner = color_point_inner,
    color_point_outer = color_point_outer,
    size_point_inner = size_point_inner,
    size_point_outer = size_point_outer
  )
}

#' @rdname pth_plot_lumchroma
#' @export
#'
pth_plot_lumchroma.pth_mat <- function(x, cvd = pth_cvd_grid_deupro(),
                                       color_point_inner = "white",
                                       color_point_outer = "black",
                                       size_point_inner = 6,
                                       size_point_outer = 7,
                                       ...) {

  .data <- pth_data_cvd(x, cvd = cvd)

  pth_plot_lumchroma(
    x = .data,
    name_color_space = pth_colorspace_name(x),
    color_point_inner = color_point_inner,
    color_point_outer = color_point_outer,
    size_point_inner = size_point_inner,
    size_point_outer = size_point_outer
  )
}

#' @rdname pth_plot_lumchroma
#' @export
#'
pth_plot_lumchroma.data.frame <- function(x, name_color_space = NULL,
                                          color_point_inner = "white",
                                          color_point_outer = "black",
                                          size_point_inner = 6,
                                          size_point_outer = 7,
                                          ...) {

  # validate data.frame
  plot_lumchroma(
    .data = x,
    name_color_space = name_color_space,
    color_point_inner = color_point_inner,
    color_point_outer = color_point_outer,
    size_point_inner = size_point_inner,
    size_point_outer = size_point_outer
  )
}

plot_lumchroma <- function(.data, name_color_space,
                           color_point_inner, color_point_outer,
                           size_point_inner, size_point_outer) {

  name_color_space <- name_color_space %||% "unknown"

  max_severity <- max(.data[["severity"]])

  keep_orig <- function(x) {

    # reorder hex_original according to luminance (descending)
    x[["hex"]] <- factor(x[["hex"]])
    x[["hex"]] <-
      forcats::fct_reorder(x[["hex"]], x[["luminance"]], .desc = TRUE)

    # keep only those rows where hex == hex_original
    keep <- x[["hex"]] == x[["hex_original"]]

    x[keep, ]
  }

  keep_max <- function(x) {
    # keep only those rows where severity == max_severity
    keep <- x[["severity"]] == max_severity
    x[keep, ]
  }

  gd <- ggplot2::guide_legend("original color", override.aes = list(color = NA))

  ggplot2::ggplot(.data, ggplot2::aes_string(x = "chroma", y = "luminance")) +
    ggplot2::geom_point(
      ggplot2::aes_string(fill = "hex"),
      data = keep_orig,
      shape = 21,
      size = size_point_inner
    ) +
    ggplot2::geom_point(
      ggplot2::aes_string(color = "hex"),
      size = size_point_inner
    ) +
    ggplot2::geom_point(
      data = keep_max,
      shape = 21,
      color = color_point_outer,
      size = size_point_outer
    ) +
    ggplot2::geom_point(
      data = keep_max,
      shape = 21,
      color = color_point_inner,
      size = size_point_inner
    ) +
    ggplot2::facet_wrap(facets = "condition") +
    ggplot2::scale_color_identity() +
    ggplot2::scale_fill_identity(guide = gd) +
    ggplot2::coord_fixed(ratio = 1, xlim = c(0, NA), ylim = c(0, 100)) +
    ggplot2::theme_bw() +
    ggplot2::theme(
      legend.text = ggplot2::element_text(family = "mono")
    ) +
    ggplot2::labs(
      title = "Colors under color-vision deficency of varying severity",
      subtitle =
        glue::glue(
          "Color space: {name_color_space}, ",
          "black/white circles: severity = {sprintf('%.2f', max_severity)}"
        )
    )
}
