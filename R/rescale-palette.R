#' Rescale palette
#'
#' Rescale a palette function using a rescaler function.
#'
#' @param pal_luv  `function` with S3 class `"cpath_palette_luv"`
#' @param rescaler `function` with S3 class `"cpath_rescaler"`
#'
#' @return `function` with S3 class `"cpath_palette_luv"`
#' @examples
#'   # create original palette
#'   pal_blues <- palette_bezier(mat_luv_blues)
#'
#'   # create rescaler
#'   rsc_reverse <- rescaler_linear_input(c(1, 0))
#'
#'   # create reversed palette
#'   pal_blues_reverse <- rescale_palette(pal_blues, rsc_reverse)
#'
#'   pal_blues(0)
#'   pal_blues_reverse(1)
#'
#' @export
#'
rescale_palette <- function(pal_luv, rescaler) {

  assertthat::assert_that(
    inherits(pal_luv, "cpath_palette_luv"),
    inherits(rescaler, "cpath_rescaler")
  )

  .f <- function(x) {
    pal_luv(rescaler(x))
  }

  structure(.f, class = "cpath_palette_luv")
}
