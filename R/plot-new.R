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

#' @rdname pth_plot_surface
#' @export
#'
pth_plot_surface.pth_surface <- function(x, step = 0.5, ...) {
  data <- pth_surface_data(x, step = step)
  plot_surface(data, fn_hue = x$fn_hue)
}

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

plot_surface <- function(data, fn_hue = NULL) {

  sec_axis_hue <- ggplot2::waiver()
  if (is.function(fn_hue)) {
    sec_axis_hue <- ggplot2::sec_axis(fn_hue, name = "hue")
  }

  g <-
    ggplot2::ggplot(
      data,
      ggplot2::aes_string(x = "chroma", y = "luminance")
    ) +
    ggplot2::geom_raster(ggplot2::aes_string(fill = "hex")) +
    ggplot2::scale_x_continuous(labels = abs) +
    ggplot2::scale_y_continuous(limits = c(0, 100), sec.axis = sec_axis_hue) +
    ggplot2::scale_fill_identity() +
    ggplot2::labs(
      x = "chroma",
      y = "luminance"
    ) +
    ggplot2::coord_fixed(ratio = 1) +
    ggplot2::theme_bw()

  g
}

#' Layer for control points
#'
#' @param x `data.frame` or `function` with S3 class `pth_chroma_trajectory` or
#'   `pth_palette_path`
#' @param color_light,color_dark `character` R colors to use for symbols
#'   and lines.
#' @param ... other args (not used)
#'
#' @return `list` of ggplot2 geoms.
#'
#' @export
#'
pth_layer_control_points <- function(x, ...) {
  UseMethod("pth_layer_control_points")
}

#' @rdname pth_layer_control_points
#' @export
#'
pth_layer_control_points.default <- function(x, ...) {
  stop(
    glue::glue("No method for class {class(x)}")
  )
}

#' @rdname pth_layer_control_points
#' @export
#'
pth_layer_control_points.data.frame <- function(x, color_light = "grey67",
                                                color_dark = "grey33", ...) {

  assertthat::assert_that(
    assertthat::has_name(x, "luminance"),
    assertthat::has_name(x, "chroma")
  )

  layer_control_points(
    x,
    color_light = color_light,
    color_dark = color_dark,
    ...
  )
}

#' @rdname pth_layer_control_points
#' @export
#'
pth_layer_control_points.pth_chroma_trajectory <- function(x,
                                                           color_light = "grey67",
                                                           color_dark = "grey33",
                                                           ...) {
  df <- pth_data_control_points(x)
  pth_layer_control_points(
    df,
    color_light = color_light,
    color_dark = color_dark,
    ...
  )
}

#' @rdname pth_layer_control_points
#' @export
#'
pth_layer_control_points.pth_palette_path <- function(x,
                                                      color_light = "grey67",
                                                      color_dark = "grey33",
                                                      ...) {
  df <- pth_data_control_points(x, ...)
  pth_layer_control_points(
    df,
    color_light = color_light,
    color_dark = color_dark,
    ...
  )
}

layer_control_points <- function(df, color_light = "grey67",
                                 color_dark = "grey33") {

  list(
    ggplot2::geom_point(data = df, color = color_dark, size = 7, shape = 3), # x
    ggplot2::geom_point(data = df, color = color_light, size = 7, shape = 4),# +
    ggplot2::geom_path(data = df, color = color_dark, linetype = "solid"),
    ggplot2::geom_path(data = df, color = color_light, linetype = "dashed")
  )
}

#' Layer for palette points
#'
#' @param palette `function` with S3 class `pth_palette`.
#' @param n_point `numeric` number of points to show.
#' @param color_point_inner,color_point_outer `character` colors to use
#'   to outline points.
#' @param size_point_inner,size_point_outer `numeric` size of the points.
#' @param n_line `numeric` number of segments in the connecting line.
#' @param color_line `character` `character` colors to use
#'  for the connecting line.
#' @param ... other args (not used)
#'
#' @return `list` of ggplot2 geoms.
#'
#' @export
#'
pth_layer_palette <- function(palette, n_point = 11,
                              color_point_inner = "white",
                              color_point_outer = "black",
                              size_point_inner = 6,
                              size_point_outer = size_point_inner + 1,
                              n_line = 200, color_line = color_point_outer,
                              ...) {

  assertthat::assert_that(
    inherits(palette, "pth_palette")
  )

  # TODO: use null colors as a way to turn off layer

  df_point <- pth_data_palette(palette, n_point)
  df_line <- pth_data_palette(palette, n_line)

  list(
    ggplot2::geom_path(data = df_line, color = color_line),
    ggplot2::geom_point(
      data = df_point,
      mapping = ggplot2::aes_string(fill = "hex"),
      color = color_point_outer,
      size = size_point_outer
    ),
    ggplot2::geom_point(
      data = df_point,
      mapping = ggplot2::aes_string(fill = "hex"),
      color = color_point_inner,
      size = size_point_inner,
      shape = 21
    )
  )
}

#' Plot a palette
#'
#' @inheritParams pth_plot_surface
#' @inheritParams pth_layer_control_points
#' @inheritParams pth_layer_palette
#' @param args_control `list` of additional arguments sent to
#'   [pth_layer_control_points()], set to `NULL` to suppress layer.
#' @param args_palette `list` of additional arguments sent to
#'   [pth_layer_palette()], set to `NULL` to suppress layer.
#' @param ... other args (not used)
#'
#' @inherit pth_plot_surface return
#' @export
#'
pth_plot_palette <- function(palette, ...) {
  UseMethod("pth_plot_palette")
}

#' @rdname pth_plot_palette
#' @export
#'
pth_plot_palette.default <- function(palette, ...) {
  stop(
    glue::glue("No method for class {class(palette)}")
  )
}

#' @rdname pth_plot_palette
#' @export
#'
pth_plot_palette.pth_palette_path <- function(palette, n_point = 11, step = 0.5,
                                              args_control = waiver(),
                                              args_palette = waiver(), ...) {

  g <- pth_plot_surface(palette, step = step)

  if (!is.null(args_control)) {
    args_control <- c(list(palette), args_control)
    g <- g + do.call(pth_layer_control_points, args_control)
  }

  if (!is.null(args_palette)) {
    args_palette <- c(list(palette, n_point = n_point), args_palette)
    g <- g + do.call(pth_layer_palette, args_palette)
  }

  g
}
