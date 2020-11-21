#' Create surfaces and trajectories
#'
#' @description
#' The central idea of this package is to create sequential palettes by
#' projecting a chroma trajectory onto a hue surface. Use these functions to
#' build the surfaces and trajectories.
#'
#' These functions themselves return functions.
#'
#' The function `pth_new_hue_surface()` takes one or two values of `hue` and two
#' values of `lum`. It returns a function that returns `hue` as a linear function
#' of `lum`.
#'
#' The function `pth_new_chroma_trajectory()` takes an equal number of `lum`
#' and `chroma` values. These serve as control points for a Bézier curve.
#' It returns a function that given input values `0 <= x <= 1`, returns
#' a matrix with as many rows as input values; `lum` and `chroma` are the
#' columns.
#'
#' @param lum `numeric` values for luminance
#' @param hue `numeric` values for hue (degrees), input values are **not**
#'   constrained to `0 <= hue < 360`.
#' @param chroma `numeric` values for chroma
#'
#' @return \describe{
#'   \item{`pth_new_hue_surface()`}{`function` with S3 class `pth_hue_surface`:
#'     for each given value of luminance `0 <= lum <= 100`, returns a value for
#'     hue `0 <= hue < 360`.}
#'   \item{`pth_new_chroma_trajectory()`}{`function` with S3 class
#'     `pth_chroma_trajectory`: for each given value `0 <= x <= 1`, returns a
#'     set of luminance and chroma values as matrix.}
#' }
#'
#' @examples
#'  # provide single value of hue for constant
#'  sfc <- pth_new_hue_surface(270)
#'  sfc(c(0, 50, 100))
#'
#'  # provide two values of hue to correspond to luminance values
#'  sfc <- pth_new_hue_surface(c(220, 270))
#'  sfc(c(0, 50, 100))
#'
#'  # provide other values for luminance, if you like
#'  sfc <- pth_new_hue_surface(c(220, 270), lum = c(25, 75))
#'  sfc(c(0, 50, 100))
#'
#'  # create a trajectory
#'  traj <-
#'    pth_new_chroma_trajectory(lum = c(20, 50, 80), chroma = c(0, 100, 0))
#'  traj(seq(0, 1, by = 0.1))
#'
#' @export
#'
pth_new_hue_surface <- function(hue, lum = c(0, 100)) {

}

pth_new_chroma_trajectory <- function(lum, chroma) {

}
