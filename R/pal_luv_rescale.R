#' Rescale LUV palette-function
#'
#' Rescale LUV palette function using a rescaler function.
#'
#' @param pal_luv  `function` with S3 class `"cpath_pal_luv"`
#' @param rescaler `function` with S3 class `"cpath_rescaler"`
#'
#' @return `function` with S3 class `"cpath_pal_luv"`
#' @examples
#'   # create original palette
#'   pal_blues <- pal_luv_bezier(mat_luv_blues)
#'
#'   # create rescaler
#'   rsc_reverse <- rescaler_x(c(1, 0))
#'
#'   # create reversed palette
#'   pal_blues_reverse <- pal_luv_rescale(pal_blues, rsc_reverse)
#'
#'   pal_blues(0)
#'   pal_blues_reverse(1)
#'
#' @export
#'
pal_luv_rescale <- function(pal_luv, rescaler) {

  assertthat::assert_that(
    inherits(pal_luv, "cpath_pal_luv"),
    inherits(rescaler, "cpath_rescaler")
  )

  .f <- function(x) {
    pal_luv(rescaler(x))
  }

  # keep existing spec_luv
  new_pal_luv(.f, spec_luv = spec_luv(pal_luv))
}