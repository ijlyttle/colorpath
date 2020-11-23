#' Rescale palette-function
#'
#' @param palette `function` with S3 class `pth_palette`.
#' @param tolerance `numeric` relative tolerance for distance calculations.
#' @inheritParams pth_distance_euclid
#'
#' @return `function` with the same S3 classes as `palette`
#' @export
#'
pth_palette_rescale_reverse <- function(palette) {

  rescaler <- pth_rescaler_reverse()

  palette_rescale(palette, rescaler)
}

#' @rdname pth_palette_rescale_reverse
#' @export
#'
pth_palette_rescale_euclid <- function(palette, tolerance = 1.e-4,
                                       non_luminance_weight = 1,
                                       transformer = identity, ...) {



}

#' @rdname pth_palette_rescale_reverse
#' @export
#'
pth_palette_rescale_metric <- function(palette, tolerance = 1.e-4,
                                       method = c("cie2000", "cie94", "cie1976", "cmc")) {




}

palette_rescale <- function(palette, rescaler) {

  assertthat::assert_that(
    inherits(palette, "pth_palette")
  )

  # create new function applying resclaer
  f <- function(x) {
    palette(rescaler(x))
  }

  # imbue new function with attributes of old function
  mostattributes(f) <- attributes(palette)

  f
}


