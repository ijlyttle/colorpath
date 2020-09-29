#' Rescaler functions
#'
#' Use this function to rescale a palette function, for example: clip a certain part
#' of it.
#'
#' A rescaler function:
#'
#' - must accept input between zero and one.
#' - must provide an output between zero and one.
#' - the output must to vary monotonically with the input.
#'
#' Use `rescaler_linear()` create a new palette-function that uses part
#' of the range of an existing palette-function.
#'
#' Use `rescaler_bezier()` to rescale a Bézier palette-function to be more
#' perceptually uniform. This uses fits a spline to equally-spaced points on
#' the colorpath. The Bézier function makes some optimization calculations,
#' so it may take a few seconds to run.
#'
#' @param range `numeric` values to correspond with `x = c(0, 1)`
#' @param pal_luv `cpath_pal_luv`, palette function on which the
#'   luminance range will operate
#'
#' @return A function with S3 class `cpath_rescaler`.
#' @examples
#'   # Linear input-rescaler
#'   rlin <- rescaler_x(c(0.25, 0.75))
#'
#'   # print for a preview
#'   print(rlin)
#'
#'   # evaluate
#'   rlin(c(0, 0.5, 1))
#'
#'   # Bezier rescaler
#'   rbez <- rescaler_bezier(mat_luv_blues)
#'
#'   # print for a preview
#'   print(rbez)
#'
#'   # evaluate
#'   rbez(c(0, 0.5, 1))
#'
#' @export
#'
rescaler_x <- function(range) {

  assertthat::assert_that(
    is.numeric(range),
    identical(length(range), 2L),
    range[1] >= 0,
    range[1] <= 1,
    range[2] >= 0,
    range[2] <= 1
  )

  .f <- function(x) {
    range[1] + x * (range[2] - range[1])
  }

  structure(.f, class = "cpath_rescaler")
}

#' @rdname rescaler_x
#' @export
#'
rescaler_luminance <- function(range, pal_luv) {

  range_input <- root_luminance(range, pal_luv)

  rescaler_x(range_input)
}

#' Find inputs to palette function for given luminances
#'
#' This assumes that the input and luminance vary monotonically
#'   so there will be exactly one root for a given luminance.
#'
#' @param lum     `numeric` values for luminance
#' @inheritParams rescaler
#'
#' @return `numeric` input values to `palette` corresponding to `lum`
#'
#' @noRd
#'
root_luminance <- function(lum, palette) {

  # function to find a single root
  root_luminance_single <- function(.lum) {

    .f <- f_root_luminance(.lum, palette)

    root_list <- stats::uniroot(.f, interval = c(0, 1))

    root_list[["root"]]
  }

  # vectorize
  vapply(lum, root_luminance_single, FUN.VALUE = 0)
}


# given a luminance target and a palette function,
#   return a function of `x` which tells us how "bad" a guess `x` is.
f_root_luminance <- function(lum, palette) {

  function(x) {

    luv_x <- palette(x)

    lum_x <- luv_x[, "l"]

    # want delta to go to zero
    unname(lum - lum_x)
  }

}

#' Bézier rescaler
#'
#' @inheritParams pal_luv_bezier
#'
#' @inherit rescaler_x
#'
#' @keywords internal
#' @export
#'
rescaler_bezier <- function(mat_luv, n = 21) {

  assertthat::assert_that(
    identical(dim(mat_luv)[2], 3L),
    msg = "luv matrix must have exactly three columns"
  )

  assertthat::assert_that(
    is.numeric(n),
    n > 1
  )

  # get the equally-spaced parameter points
  pob <- bezier::pointsOnBezier(p = mat_luv, n = n, method = "evenly_spaced")

  # create a spline-function mapping x to equally-spaced t
  x <- seq(0, 1, length.out = n)
  .f <- stats::splinefun(x = x, y = pob$t, method = "natural")

  structure(.f, class = "cpath_rescaler")
}

#' @export
#'
print.cpath_rescaler <- function(x, ...) {

  .x <- seq(0, 1, by = 0.2)
  .y <- x(.x)

  vals_x <- glue::glue_collapse(sprintf("%.3f", .x), sep = " ")
  vals_y <- glue::glue_collapse(sprintf("%.3f", .y), sep = " ")

  msg_x <- glue::glue("   input: {vals_x}")
  msg_y <- glue::glue("  output: {vals_y}")

  cat("Rescaler function:\n")
  cat(msg_x, "\n")
  cat(msg_y, "\n")

  invisible(x)
}
