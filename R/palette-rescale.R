#' Rescale palette-function
#'
#' @description
#' The purpose of these functions is to rescale the input to palette functions.
#' Inputs to palette functions must be `0 <= x <= 1`. A rescaler maps `x` to
#' `x'` such that `0 <= x' <= 1`; a mapping function will be monotonic.
#'
#' `pth_palette_rescale_reverse()` reverses the input, so that `x` maps to
#'   `1 - x`.
#'
#'  The other rescaling functions, `pth_palette_rescale_euclid()` and
#'  `pth_palette_rescale_metric()`, are used so to rescale a palette function
#'  such that a constant change in input, `x`, results in a constant change
#'  in color, for a given definition of "change in color".
#'
#' @param palette `function` with S3 class `pth_palette`.
#' @param tolerance `numeric` relative tolerance for distance calculations.
#' @inheritParams pth_distance_euclid
#'
#' @return `function` with the same S3 classes as `palette`
#' @export
#'
pth_palette_rescale_reverse <- function(palette) {

  assertthat::assert_that(
    inherits(palette, "pth_palette")
  )

  rescaler <- pth_rescaler_reverse()

  palette_rescale(palette, rescaler)
}

#' @rdname pth_palette_rescale_reverse
#' @export
#'
pth_palette_rescale_euclid <- function(palette, tolerance = 1.e-4,
                                       non_luminance_weight = 1,
                                       transformer = identity, ...) {

  assertthat::assert_that(
    inherits(palette, "pth_palette"),
    assertthat::is.number(tolerance),
    assertthat::is.number(non_luminance_weight),
    is.function(transformer)
  )

  rescaler <-
    pth_rescaler_euclid(
      palette,
      tolerance = tolerance,
      non_luminance_weight = non_luminance_weight,
      transformer = transformer,
      ...
    )

  palette_rescale(palette, rescaler)
}

#' @rdname pth_palette_rescale_reverse
#' @export
#'
pth_palette_rescale_metric <- function(palette, tolerance = 1.e-4,
                                       method = c("cie2000", "cie94", "cie1976", "cmc")) {

  assertthat::assert_that(
    inherits(palette, "pth_palette"),
    assertthat::is.number(tolerance)
  )

  method <- match.arg(method)

  rescaler <-
    pth_rescaler_metric(palette, tolerance = tolerance, method = method)

  palette_rescale(palette, rescaler)
}

palette_rescale <- function(palette, rescaler) {

  assertthat::assert_that(
    inherits(palette, "pth_palette"),
    is.function(rescaler)
  )

  # create new function applying resclaer
  f <- function(x) {
    palette(rescaler(x))
  }

  # imbue new function with attributes of old function
  mostattributes(f) <- attributes(palette)

  f
}


