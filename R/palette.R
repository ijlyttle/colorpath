#' Create palette function from path
#'
#' @description
#' A palette function converts numerical inputs into colors. A path is defined
#' by the intersection of a hue-surface with a chroma-trajectory, in a given
#' color space.
#'
#' Use `pth_new_palette_path()` to create a palette function by defining a path.
#' You can create a `trajectory` using [pth_new_chroma_trajectory()]; you can
#' create a `surface` using [pth_new_hue_surface()]. Use the `constructor` to
#' identify which color space the palette function will use.
#'
#' Use `pth_new_palette_hex()` to create a palette function using a vector of
#' hex codes. The function will use a spline defined in the color space
#' identified by the `transformer` argument.
#'
#' @param trajectory `function` with class `pth_chroma_trajectory`, used to
#'   define the path that the luminance and chroma follow.
#' @param surface `function` with class `pth_hue_surface`, used to define the
#'   hue as a function of luminance.
#' @param constructor `function` that constructs a `pth_mat`, used to
#'  identify the color space for the output matrix.
#' @inheritParams pth_new_hex
#' @inheritParams pth_distance_euclid
#' @param ... other arguments passed to `constructor` or `transformer`
#'
#' @return `function` with S3 classes `pth_palette_path`, `pth_palette`. For
#' each given value (`0 <= x <= 1`), returns a matrix, in the color space
#' associated with `constructor`, with a row for each value.
#'
#' @export
#'
pth_new_palette_path <- function(trajectory, surface,
                                 constructor = pth_new_cieluv, ...) {

  assertthat::assert_that(
    inherits(trajectory, "pth_chroma_trajectory"),
    inherits(surface, "pth_hue_surface"),
    is.function(constructor)
  )

  f <- function(x) {

    # use trajectory and surface to get polar coordinates
    lum_chroma <- trajectory(x)
    hue <- surface(lum_chroma[, 1])
    polar <- cbind(lum_chroma, hue)

    # convert to Cartesian
    cart <- pth_to_cartesian(polar)

    # put into color space
    constructor(cart, ...)
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

#' Create surfaces and trajectories
#'
#' @description
#' The central idea of this package is to create sequential palettes by
#' projecting a chroma trajectory onto a hue surface. Use these functions to
#' build the surfaces and trajectories.
#'
#' These functions themselves return functions.
#'
#' The function `pth_new_hue_surface()` takes one or two values of `hue` and two
#' values of `lum`. It returns a function that returns `hue` as a linear function
#' of `lum`.
#'
#' The function `pth_new_chroma_trajectory()` takes an equal number of `lum`
#' and `chroma` values. These serve as control points for a Bézier curve.
#' It returns a function that given input values `0 <= x <= 1`, returns
#' a matrix with as many rows as input values; `lum` and `chroma` are the
#' columns.
#'
#' One thing to keep in mind when designing a trajectory: although it is a good
#' thing to keep the trajectory within the gamut, it is not necessary
#' (or perhaps not even desirable) to keep the control points within the gamut.
#'
#' @param lum `numeric` values for luminance
#' @param hue `numeric` values for hue (degrees), input values are **not**
#'   constrained to `0 <= hue < 360`.
#' @param chroma `numeric` values for chroma.
#'
#' @return \describe{
#'   \item{`pth_new_hue_surface()`}{`function` with S3 class `pth_hue_surface`:
#'     for each given value of luminance `0 <= lum <= 100`, returns a value for
#'     hue `0 <= hue < 360`.}
#'   \item{`pth_new_chroma_trajectory()`}{`function` with S3 class
#'     `pth_chroma_trajectory`: for each given value `0 <= x <= 1`, returns a
#'     set of luminance and chroma values as matrix.}
#' }
#'
#' @examples
#'  # create surfaces
#'  # provide single value of hue for constant
#'  sfc <- pth_new_hue_surface(270)
#'  sfc(c(0, 50, 100))
#'
#'  # provide two values of hue to correspond to luminance values
#'  sfc <- pth_new_hue_surface(c(220, 270))
#'  sfc(c(0, 50, 100))
#'
#'  # provide other values for luminance, if you like
#'  sfc <- pth_new_hue_surface(c(220, 270), lum = c(25, 75))
#'  sfc(c(0, 25, 50, 75, 100))
#'
#'  # create a trajectory
#'  traj <-
#'    pth_new_chroma_trajectory(chroma = c(0, 100, 0), lum = c(20, 50, 80))
#'  traj(seq(0, 1, by = 0.1))
#'
#' @export
#'
pth_new_hue_surface <- function(hue, lum = c(0, 100)) {

  # validate hue
  assertthat::assert_that(
    is.numeric(hue),
    length(hue) <= 2,
    length(hue) > 0
  )

  # validate lum
  assertthat::assert_that(
    is.numeric(lum),
    length(lum) == 2
  )

  assertthat::assert_that(
    !identical(lum[1], lum[2]),
    msg = "values of input_lum have to be distinct from each other"
  )

  # if only one hue specified, impute the other
  if (identical(length(hue), 1L)) {
    hue[2] <- hue[1]
  }

  f <- function(.lum) {

    x <- (.lum - lum[1]) / (lum[2] - lum[1])

    hue <- hue[1] + x * (hue[2] - hue[1])

    hue %% 360
  }

  structure(f, class = "pth_hue_surface")
}

#' @rdname pth_new_hue_surface
#' @export
#'
pth_new_chroma_trajectory <- function(chroma, lum) {

  # validate chroma, lum
  assertthat::assert_that(
    is.numeric(chroma),
    is.numeric(lum),
    identical(length(chroma), length(lum)),
    length(chroma) > 0,
    all(chroma >= 0)
  )

  control_points <- matrix(c(lum, chroma), ncol = 2, byrow = FALSE)
  dimnames(control_points) <- list(NULL, c("lum", "chroma"))

  # create trajectory function
  f <- function(x) {
    mat <- bezier::bezier(t = x, p = control_points)
    dimnames(mat) <- list(NULL, c("lum", "chroma"))

    mat
  }

  structure(
    f,
    class = "pth_chroma_trajectory",
    control_points = control_points
  )
}

