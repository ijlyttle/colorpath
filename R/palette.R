#' Create palette function from path
#'
#' @description
#' A palette function converts numerical inputs into colors. A path is defined
#' by the intersection of a hue-surface with a chroma-trajectory, in a given
#' color space.
#'
#' Use `pth_new_palette_path()` to create a palette function by defining a path.
#' You can create a `trajectory` using [pth_new_trajectory()]; you can
#' create a `surface` using [pth_new_surface()]. Use the `constructor` to
#' identify which color space the palette function will use.
#'
#' Use `pth_new_palette_hex()` to create a palette function using a vector of
#' hex codes. The function will use a spline defined in the color space
#' identified by the `transformer` argument.
#'
#' @param trajectory `function` with class `pth_trajectory`, used to
#'   define the path that the luminance and chroma follow.
#' @param surface `function` with class `pth_surface`, used to define the
#'   hue as a function of luminance.
#' @inheritParams pth_new_hex
#' @inheritParams pth_distance_euclid
#' @param ... other arguments passed to `transformer`
#'
#' @return `function` with S3 class `pth_palette`. For
#' each given value (`0 <= x <= 1`), returns a matrix, in the color space
#' associated with `surface`, with a row for each value.
#' `pth_new_palette_path()` returns an object with an additional class
#' `pth_palette_path`; `pth_new_palette_hex()`: `pth_palettte_hex`.
#'
#' @export
#'
pth_new_palette_path <- function(trajectory, surface) {

  assertthat::assert_that(
    inherits(trajectory, "pth_trajectory"),
    inherits(surface, "pth_surface")
  )

  constructor <- pth_creator(surface$colors)

  f <- function(x) {

    # use trajectory and surface to get polar coordinates
    lum_chroma <- trajectory(x)
    hue <- surface$fn_hue(lum_chroma[, 1])
    polar <- cbind(lum_chroma, hue)

    # convert to Cartesian
    cart <- pth_to_cartesian(polar)

    # put into color space
    constructor(cart)
  }

  # putting the attributes into lists, in anticipation of concatenating
  # attributes when we join palettes
  structure(
    f,
    class = c("pth_palette_path", "pth_palette"),
    control_points = list(attr(trajectory, "control_points")),
    surface = list(surface)
  )
}

#' @rdname pth_new_palette_path
#' @export
#'
pth_new_palette_hex <- function(hex, transformer = pth_to_cieluv, ...) {

  hex <- pth_to_hex(hex)

  # transform the hex codes to the color space
  nodes_cartesian <- transformer(hex, ...)

  # make sure we have enough colors and the transformer is good
  assertthat::assert_that(
    length(hex) > 1
  )

  assertthat::assert_that(
    inherits(nodes_cartesian, "pth_mat"),
    msg = "`transformer` function does not output `pth_mat`"
  )

  nodes_x <- seq(0, 1, length.out = length(hex))

  spline_lum <- stats::splinefun(nodes_x, nodes_cartesian[, 1])
  spline_a   <- stats::splinefun(nodes_x, nodes_cartesian[, 2])
  spline_b   <- stats::splinefun(nodes_x, nodes_cartesian[, 3])

  # TODO: think about how to construct a surface
  #       - such a surface may contain a branch-cut, but OK

  f <- function(x) {

    cartesian <- cbind(spline_lum(x), spline_a(x), spline_b(x))

    # use the color space defined in nodes_cartesian
    pth_mat_replace_data(nodes_cartesian, cartesian)
  }

  structure(
    f,
    class = c("pth_palette_hex", "pth_palette"),
    nodes = list(hex)
  )
}



