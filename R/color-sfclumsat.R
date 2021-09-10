#' Get color using surface, luminance, and saturation
#'
#' @param surface object with S3 class `pth_surface`.
#' @param lum `double` luminance of the color, `0 <= lum <= 100`.
#' @param sat `double` saturation of the color, `0 <= sat <= 1`.
#'
#' @return Object with S3 class `pth_mat`, describing a color.
#' @examples
#'   sfc_blue <- pth_new_surface("#0000FF")
#'   color <- pth_color_sfclumsat(sfc_blue, lum = 50, sat = 1)
#'
#'   pth_to_hex(color)
#' @export
#'
pth_color_sfclumsat <- function(surface, lum, sat = 1) {

  assertthat::assert_that(
    class(surface) == "pth_surface",
    assertthat::is.number(lum),
    assertthat::is.number(sat),
    lum >= 0,
    lum <= 100,
    sat >= 0,
    sat <= 1
  )

  chroma <- surface$fn_max_chroma(lum) * sat
  hue <- surface$fn_hue(lum)

  mat_polar <- matrix(c(lum, chroma, hue), ncol = 3)
  mat_cartesian <- pth_to_cartesian(mat_polar)

  # use the same color space as the surface
  color <- pth_creator(surface$colors)(mat_cartesian)

  color
}
