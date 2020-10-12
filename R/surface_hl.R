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

  f <- function(lum) {

    x <- (lum - input_lum[1]) / (input_lum[2] - input_lum[1])

    hue <- range_hue[1] + x * (range_hue[2] - range_hue[1])

    hue %% 360
  }

  structure(f, class = "cpath_surface_hl")
}

#' Create HCL data-frame using HL surface
#'
#' @param df_cl `data.frame` with columns `c`, `l`.
#' @param sfc `function` created using `surface_hl()`.
#'
#' @return `tibble` with columns `h`, `c`, `l`
#' @examples
#'   sfc_blues_multi <- surface_hl(c(240, 260))
#'   df_cl <- data.frame(l = c(20, 50, 80), c = c(0, 150, 0))
#'
#'   df_hcl(df_cl, sfc_blues_multi)
#' @export
#'
df_hcl <- function(df_cl, sfc) {

  assertthat::assert_that(
    is.data.frame(df_cl),
    assertthat::has_name(df_cl, c("c", "l")),
    is.function(sfc)
  )

  tibble::tibble(
    h = sfc(df_cl[["l"]]),
    c = df_cl[["c"]],
    l = df_cl[["l"]]
  )
}

#' Create HCL matrix along chroma
#'
#' @param hue `numeric`
#' @param luminance `numeric`
#' @param max_chroma `numeric`
#' @param step `numeric`
#'
#' @return `matrix` with columns named `h`, `c`, `l`
#'
#' @noRd
#'
mat_chroma <- function(hue, luminance, max_chroma, step) {

  n <- floor(max_chroma / step)

  # what if there is nothing to send back?
  if (identical(n, 0L)) {
    return(NULL)
  }

  hue <- rep(hue, n)
  chroma <- seq(from = step / 2, by = step, length.out = n)
  luminance <- rep(luminance, n)

  matrix(
    c(hue, chroma, luminance),
    ncol = 3,
    byrow = FALSE,
    dimnames = list(NULL, c("h", "c", "l"))
  )

}

#' Create matrix for HL surface
#'
#' @param sfc `function` created using `surface_hl()`
#' @param step `integer` size of step in hue and chroma
#'
#' @return `matrix` with columns named `h`, `c`, `l`
#'
#' @noRd
#'
mat_surface_hl <- function(sfc, step = 0.5) {

  assertthat::assert_that(
    inherits(sfc, "cpath_surface_hl"),
    assertthat::is.number(step)
  )

  n_lum <- floor(100 / step)

  luminance <- seq(from = step / 2, by = step, length.out = n_lum)
  hue <- sfc(luminance)


  .f <- function(hue, luminance, step) {
    max_chroma <- colorspace::max_chroma(hue, luminance)

    local <- mat_chroma(hue, luminance, max_chroma, step)

    local
  }

  tmp <- purrr::map2(hue, luminance, .f, step = step)

  do.call(rbind, tmp)
}

#' Create HCL data frame for surface
#'
#' @inheritParams df_hcl
#' @param step `numeric`, step size in hue, luminance
#'
#' @return `tibble` with columns `h`, `c`, `l`, `hex`.
#'
#' @examples
#'   # multi hue
#'   sfc_blues_multi <- surface_hl(c(240, 260))
#'   df_sfc <- data_surface_hl(sfc_blues_multi) # lots of rows!
#' @export
#'
data_surface_hl <- function(sfc, step = 0.5) {

  mat <- mat_surface_hl(sfc, step)

  hex <- farver::encode_colour(mat, from = "hcl")

  df <- tibble::as_tibble(mat)

  df$hex <- hex

  df
}

#' Plot surface
#'
#' @inheritParams data_surface_hl
#'
#' @inherit plot_cl return
#'
#' @examples
#'   # multi hue
#'   sfc_blues_multi <- surface_hl(c(240, 260))
#'   plot_surface_hl(sfc_blues_multi)
#'
#' @export
#'
plot_surface_hl <- function(sfc, step = 0.5) {

  df_sfc <- data_surface_hl(sfc, step)

  g <-
    ggplot2::ggplot(
      df_sfc,
      ggplot2::aes_string(x = "c", y = "l", fill = "hex")
    ) +
    ggplot2::geom_raster() +
    ggplot2::scale_y_continuous(
      sec.axis = ggplot2::sec_axis(sfc, name = "hue")
    ) +
    ggplot2::xlim(0, NA) +
    ggplot2::ylim(0, 100) +
    ggplot2::scale_fill_identity() +
    ggplot2::labs(
      x = "chroma",
      y = "luminance"
    ) +
    ggplot2::coord_fixed(ratio = 1) +
    ggplot2::theme_bw()

  g
}

