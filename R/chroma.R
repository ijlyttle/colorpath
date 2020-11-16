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
#' @inheritParams pth_to_cielab
#'
#' @return `logical` for each color indicates if in-gamut
#' @examples
#'   pth_in_gamut("#663399")
#' @export
#'
pth_in_gamut <- function(color) {
  x_gamut(color) <= 0
}

pth_max_chroma <- function(mat) {

  len <- nrow(mat)

  if (identical(len, 0L)) {
    return(NULL)
  }

  # for loop it is, then
  for (i in seq(len)) {

  }
}

#' Determine gamut-distance for a chroma, given a color
#'
#' @param chroma `numeric` value for chroma, expressed using color space
#'   for `mat`.
#' @inheritParams pth_to_hex
#'
#' @return `double` indicating "distance" from gamut surface,
#'   negative indicates inside gamut.
#'
#' @keywords internal
#' @export
#'
x_gamut_chroma <- function(chroma, mat) {

  # TODO: potential to memoise
  polar <- pth_to_polar(mat)

  polar[, 2] <- chroma
  mat_new <- pth_to_cartesian(polar, chroma_min = 0)

  mat[, 2:3] <- mat_new[, 2:3]

  x_gamut(mat)
}

#' Get single root
#'
#' @param mat `matrix` with S3 class `pth_mat` and **exactly** one row.
#'
#' @return `double` maximum chroma value for RGB gamut, given luminance and
#'   hue in `mat`.
#'
#' @noRd
#'
root_chroma <- function(mat) {

  root <-
    stats::uniroot(
      x_gamut_chroma,
      interval = c(0, 200),
      mat = mat,
      extendInt = "yes"
    )

  root$root
}
