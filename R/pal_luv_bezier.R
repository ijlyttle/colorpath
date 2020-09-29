#' Create palette-function using Bézier spline
#'
#' Thus takes a `matrix` of LUV coordinates and returns a palette-function
#' (that returns LUV values) based on a Bézier spline. You can get an `LUV`
#' matrix using the function [as_mat_luv()].
#'
#' @param mat_luv `matrix` of LUV coordinates, control-points for Bézier spline.
#' @param rescale_path `logical` indicating to rescale the palette function to be
#'   perceptually-uniform with respect to the input.
#' @param n `numeric` if `rescale_path`, number of equally-spaced Bézier points to calculate.
#'
#' @return A function with S3 class `cpath_palette`.
#'
#' @examples
#'  # create palette-function
#'  pb <- pal_luv_bezier(mat_luv_blues)
#'
#'  # evaluate
#'  pb(c(0, 0.5, 1))
#'
#' @export
#'
pal_luv_bezier <- function(mat_luv, rescale_path = TRUE, n = 21) {

  assertthat::assert_that(
    is.matrix(mat_luv)
  )

  # create unscaled function
  .f <- function(x) {
    mat <- bezier::bezier(t = x, p = mat_luv)
    dimnames(mat) <- list(NULL, c("l", "u", "v"))

    mat
  }
  .f <- structure(.f, class = "cpath_pal_luv")

  # if need be, rescale
  if (rescale_path) {
    .r <- rescaler_bezier(mat_luv, n)
    .f <- rescale_pal_luv(.f, .r)
  }

  .f
}