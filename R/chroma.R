#' Get "distance" from gamut surface
#'
#' @inheritParams pth_to_hex
#'
#' @return `double` for each color: positive indicates out-of-gamut,
#'   negative or zero indicates in-gamut
#'
#' @noRd
#'
x_gamut <- function(color) {

  rgb <- to_rgb(color)
  dimnames(rgb) <- NULL

  # positive outside of 0 <= x <= 255
  .f <- function(x) {
    abs(x - 127.5) - 127.5
  }

  x <- .f(rgb)

  pmax(x[, 1], x[, 2], x[, 3])
}

#' Determine if color is in RGB gamut
#'
#' @inheritParams pth_to_hex
#'
#' @return `logical` for each color indicates if in-gamut
#' @examples
#'   pth_in_gamut("#663399")
#' @export
#'
pth_in_gamut <- function(color) {
  x_gamut(color) <= 0
}
