#' Coerce to hex-code palette-function
#'
#' @description
#' The "standard" palette function in this package takes a numeric input
#' (`0 <= x <=1`) and returns a matrix output (LUV coordinates).
#'
#' Perhaps you want a palette function to return hex-codes; use `as_pal_hex()`.
#'
#' Perhaps you want a palette function to act as a discrete-palette function;
#' use `as_pal_disc()`.
#'
#' It is important to keep in mind that each of these functions returns a
#' **function**.
#'
#' @inherit rescale_pal_luv params
#' @param pal `function` that takes a numeric input and returns a color value.
#'
#' @return `function`:
#' \describe{
#'   \item{`as_pal_hex()`}{return a palette function that returns hex-codes.}
#'   \item{`as_pal_disc()`}{return a discrete-palette function which takes
#'    an integer input `n`, and returns `n` equally-distributed values.}
#' }
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
#'   # create a discrete hex palette-function
#'   pal_blues_hex_disc <- as_pal_disc(pal_blues_hex)
#'
#'   # evaluate
#'   pal_blues_hex_disc(6)
#'
#' @export
#'
as_pal_hex <- function(pal_luv) {

  assertthat::assert_that(
    inherits(pal_luv, "cpath_pal_luv")
  )

  function(x) {
    farver::encode_colour(pal_luv(x), from = "luv")
  }
}

#' @rdname as_pal_hex
#' @export
#'
as_pal_disc <- function(pal) {
  function(n) {
    pal(seq(0, 1, length.out = n))
  }
}
