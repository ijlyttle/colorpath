#' Plot chroma-luminance plane
#'
#' @inheritParams data_hcl
#' @param label_hue `logical` indicates to add a text layer showing the
#'   hue each control point.
#'
#' @return Object with S3 class `gg`.
#' @export
#'
plot_cl <- function(pal_luv, n = 11, label_hue = FALSE) {

  data_hcl <- data_hcl(pal_luv, n = n)

  g <-
    ggplot2::ggplot(
      data_hcl,
      ggplot2::aes_string(
        x = "chroma",
        y = "luminance",
        color = "hex",
        shape = "type")
    ) +
    ggplot2::geom_point(size = 4) +
    # path will show up only for the control points because
    #   the color is constant for the control points
    ggplot2::geom_path(linetype = 2, alpha = 0.5) +
    ggplot2::ylim(0, 100) +
    ggplot2::scale_color_identity() +
    ggplot2::scale_shape_manual(
      name = "",
      values = c(`palette` = 19, `max chroma` = 1,`control point` = 4)
    ) +
    ggplot2::coord_fixed(ratio = 1) +
    ggplot2::theme_bw()

    if (label_hue) {
      g <-
        g +
        ggplot2::geom_text(
          ggplot2::aes_string(label = "label"),
          hjust = -0.25,
          size = 3.5
        )
    }

  g
}

#' Get HCL data
#'
#' This is in support of plots to show chroma-luminance plane and
#' hue-chroma plane.
#'
#' @inheritParams pal_luv_rescale
#' @param n, `integer` number of points to generate for the palette
#'
#' @return [tibble::tibble()] with variables `type`, `hex`, `hue`, `chroma`,
#'   `luminance`.
#'
#' @export
#'
#'
data_hcl <- function(pal_luv, n = 11) {

  # get LUV matrix from palette
  mat_luv_palette <- pal_luv(seq(0, 1, length.out = n))

  # get HCL values for palette
  mat_hcl_palette <-
    farver::convert_colour(mat_luv_palette, from = "luv", to = "hcl")

  # get HCL values for max-chroma
  mat_hcl_maxchroma <- mat_hcl_palette
  mat_hcl_maxchroma[, "c"] <-
    colorspace::max_chroma(
      h =  mat_hcl_maxchroma[, "h"],
      l =  mat_hcl_maxchroma[, "l"]
    )

  # get HCL values for control points
  mat_luv_control <- spec_luv(pal_luv)
  mat_hcl_control <-
    farver::convert_colour(mat_luv_control, from = "luv", to = "hcl")

  # data frame
  df <- rbind(
    df_hcl(mat_hcl_palette, type = "palette", use_hex = TRUE),
    df_hcl(mat_hcl_maxchroma, type = "max chroma", use_hex = TRUE),
    df_hcl(mat_hcl_control, type = "control point", use_hex = FALSE)
  )

  df
}

df_hcl <- function(mat_hcl, type, use_hex) {

  hex <- "#777777"
  if (use_hex) {
    hex <- farver::encode_colour(mat_hcl, from = "hcl")
  }

  label <- ""
  if (type == "control point") {
    label = sprintf("hue = %.0f", mat_hcl[, "h"])
  }

  tibble::tibble(
    type = factor_type(type),
    hex = hex,
    hue = mat_hcl[, "h"],
    chroma = mat_hcl[, "c"],
    luminance = mat_hcl[, "l"],
    label = label
  )

}

factor_type <- function(x) {

  type_levels <- c("control point", "palette", "max chroma")

  factor(x, levels = type_levels)
}

