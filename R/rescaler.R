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
#' the colorpath. The Bézier function carries some optimizations, so it may
#' take a few seconds to run.
#'
#' @param x0 `numeric` value to correspond with `x = 0`
#' @param x1 `numeric` value to correspond with `x = 1`
#'
#' @inheritParams palette_bezier
#' @param n `numeric` number of equally-spaced Bézier points to calculate
#'
#' @return A function with S3 class `cpath_rescaler`.
#' @examples
#'   # Linear rescaler
#'   rlin <- rescaler_linear(0.25, 0.75)
#'
#'   # print for a preview
#'   print(rlin)
#'
#'   # evaluate
#'   rlin(c(0, 0.5, 1))
#'
#'   # Bezier rescaler
#'   rbez <- rescaler_bezier(mat_luv_blues)
#'   # print for a preview
#'   print(rbez)
#'
#'   # evaluate
#'   rbez(c(0, 0.5, 1))
#'
#' @export
#'
rescaler_linear <- function(x0, x1) {

  assertthat::assert_that(
    x0 >= 0,
    x0 <= 1,
    x1 >= 0,
    x1 <= 1
  )

  .f <- function(x) {
    x0 + x * (x1 - x0)
  }

  structure(.f, class = "cpath_rescaler")
}

#' @rdname rescaler_linear
#' @export
#'
rescaler_bezier <- function(luv, n = 21) {

  assertthat::assert_that(
    identical(dim(luv)[2], 3L),
    msg = "luv matrix must have exactly three columns"
  )

  assertthat::assert_that(
    is.numeric(n),
    n > 1
  )

  # get the equally-spaced parameter points
  pob <- bezier::pointsOnBezier(p = luv, n = n, method = "evenly_spaced")

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
