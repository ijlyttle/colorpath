#' Create HL surface-function
#'
#' Creates a function that defines hue in terms of luminance.
#'
#' The `range_hue` you specify need not be between 0 and 360 - in fact you
#' might not want them to be if you want to define a surface that crosses the
#' hue branch-cut. The surface function will return hue values between 0 and 360.
#'
#' @param range_hue `numeric` vector, length 1 or 2, specifies the
#'   hue at the luminance values specified in `input_lum`. If only one
#'   hue is specified, this will return a function for constant hue.
#' @param input_lum `numeric` vector, length 2, specifies the
#'   luminance values corresponding to `range_hue`.
#'
#' @return `function` that takes luminance values, returns hue values.
#' @examples
#'   # single hue
#'   sfc_blues_single <- surface_hl(250)
#'   sfc_blues_single(c(0, 50, 100))
#'
#'   # multi hue
#'   sfc_blues_multi <- surface_hl(c(240, 260))
#'   sfc_blues_multi(c(0, 50, 100))
#'
#' @export
#'
surface_hl <- function(range_hue, input_lum = c(0, 100)) {

  # validation for range_hue
  assertthat::assert_that(
    is.numeric(range_hue),
    length(range_hue) <= 2,
    length(range_hue) > 0
  )

  # validation for input_lum
  assertthat::assert_that(
    is.numeric(input_lum),
    length(input_lum) == 2
  )

  assertthat::assert_that(
    input_lum[1] != input_lum[2],
    msg = "values of input_lum have to be distinct from each other"
  )

  # if only one hue specified, impute the other
  if (identical(length(range_hue), 1L)) {
    range_hue[2] <- range_hue[1]
  }

  function(lum) {

    x <- (lum - input_lum[1]) / (input_lum[2] - input_lum[1])

    hue <- range_hue[1] + x * (range_hue[2] - range_hue[1])

    hue %% 360
  }
}

#' Create HCL data-frame using HL surface
#'
#' @param df_cl `data.frame` with columns `c`, `l`.
#' @param surface_hl `function` created using `surface_hl()`.
#'
#' @return `tibble` with columns `h`, `c`, `l`
#' @examples
#'   sfc_blues_multi <- surface_hl(c(240, 260))
#'   df_cl <- data.frame(l = c(20, 50, 80), c = c(0, 150, 0))
#'
#'   df_hcl(df_cl, sfc_blues_multi)
#' @export
#'
df_hcl <- function(df_cl, surface_hl) {

  assertthat::assert_that(
    is.data.frame(df_cl),
    assertthat::has_name(df_cl, c("c", "l")),
    is.function(surface_hl)
  )

  tibble::tibble(
    h = surface_hl(df_cl[["l"]]),
    c = df_cl[["c"]],
    l = df_cl[["l"]]
  )
}
