#' Coerce to hex-code palette-function
#'
#' Most of the other
#'
#' @inherit rescale_palette params
#'
#' @return `function` given a numeric input `(0 <= x <= 1)`, return hex-codes
#'
#' @examples
#'   # create LUV palette-function
#'   pal_blues <- palette_bezier(mat_luv_blues)
#'
#'   # create hex palette-function
#'   pal_blues_hex <- as_pal_hex(pal_blues)
#'
#'   # evaluate
#'   pal_blues_hex(seq(0, 1, by = 0.2))
#'
#' @export
#'
as_pal_hex <- function(pal_luv) {

  assertthat::assert_that(
    inherits(pal_luv, "cpath_palette_luv")
  )

  function(x) {
    farver::encode_colour(pal_luv(x), from = "luv")
  }
}
