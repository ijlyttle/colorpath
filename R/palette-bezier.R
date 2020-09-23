#' Create palette-function using Bézier curves
#'
#' @param luv `matrix` of LUV coordinates, control-points for Bézier spline.
#'
#' @return A function with S3 class `cpath_palette`.
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
