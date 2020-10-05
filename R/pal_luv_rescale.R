#' Rescale LUV palette-function
#'
#' Rescale LUV palette function using a rescaler function.
#'
#' @param pal_luv  `function` with S3 class `"cpath_pal_luv"`
#' @param rescaler `function` with S3 class `"cpath_rescaler"`
#' @inheritParams rescaler_x
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

#' @rdname pal_luv_rescale
#' @export
#'
pal_luv_rescale_x <- function(pal_luv, range) {

  rescaler <- rescaler_x(range)

  pal_luv_rescale(pal_luv, rescaler)
}


#' @rdname pal_luv_rescale
#' @export
#'
pal_luv_rescale_lum <- function(pal_luv, range) {

  rescaler <- rescaler_lum(range, pal_luv)

  pal_luv_rescale(pal_luv, rescaler)
}
