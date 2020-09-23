#' Create palette-function using Bézier spline
#'
#' Thus takes a `matrix` of LUV coordinates and returns a palette-function
#' (that returns LUV values) based on a Bézier spline. You can get an `LUV`
#' matrix using the function [luv()].
#'
#' @param luv `matrix` of LUV coordinates, control-points for Bézier spline.
#'
#' @return A function with S3 class `cpath_palette`.
#'
#' @examples
#'  # matrix of LUV values
#'  mat_luv <- matrix(
#'    c(
#'      20,  50, 80, # l
#'      0,  -46,  0, # u
#'      0,   56,  0  # v
#'    ),
#'    ncol = 3L,
#'    byrow = FALSE,
#'    dimnames = list(NULL, c("l", "u", "v"))
#'  )
#'
#'  # create palette-function
#'  pb <- palette_bezier(mat_luv)
#'
#'  # evaluate
#'  pb(c(0, 0.5, 1))
#'
#' @export
#'
palette_bezier <- function(luv) {

  assertthat::assert_that(
    is.matrix(luv)
  )

  .f <- function(x) {
    mat <- bezier::bezier(t = x, p = luv)
    dimnames(mat) <- list(NULL, c("l", "u", "v"))

    mat
  }

  structure(.f, class = "cpath_palette_luv")
}
