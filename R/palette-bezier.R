#' Create palette-function using Bézier spline
#'
#' Thus takes a `matrix` of LUV coordinates and returns a palette-function
#' (that returns LUV values) based on a Bézier spline. You can get an `LUV`
#' matrix using the function [luv()].
#'
#' @param mat_luv `matrix` of LUV coordinates, control-points for Bézier spline.
#'
#' @return A function with S3 class `cpath_palette`.
#'
#' @examples
#'  # create palette-function
#'  pb <- palette_bezier(mat_luv_blues)
#'
#'  # evaluate
#'  pb(c(0, 0.5, 1))
#'
#' @export
#'
palette_bezier <- function(mat_luv) {

  assertthat::assert_that(
    is.matrix(mat_luv)
  )

  .f <- function(x) {
    mat <- bezier::bezier(t = x, p = mat_luv)
    dimnames(mat) <- list(NULL, c("l", "u", "v"))

    mat
  }

  structure(.f, class = "cpath_palette_luv")
}
