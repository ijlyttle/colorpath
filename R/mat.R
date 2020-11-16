#' Convert between Cartesian and polar coordinates
#'
#' @param mat_polar `matrix` with columns for luminance, chroma, hue.
#' @param mat_cartesian `matrix` with columns for luminance,
#'   blue-yellow, green-red.
#' @param chroma_min `numeric` "trick" used to preserve the hue at zero chroma,
#'   using a value small enough not to affect color-perception.
#'
#' @return `matrix` with same dimension as the input matrix.
#' @examples
#'   polar <- matrix(c(50, 0, 45), ncol = 3)
#'   cart <- pth_to_cartesian(polar)
#'   polar_new <- pth_to_polar(cart)
#'   # notice the chroma value has an artificial floor
#'   # using an imperceptible (color) difference
#'   print(polar)
#'   print(cart)
#'   print(polar_new)
#' @export
#'
pth_to_cartesian <- function(mat_polar, chroma_min = 1.e-4) {

  assertthat::assert_that(
    is_mat(mat_polar)
  )

  lum <- mat_polar[, 1]
  chr <- pmax(mat_polar[, 2], chroma_min)
  hue <- to_radians(mat_polar[, 3])

  # new columns are lum, a, b
  mat <- cbind(
    lum,
    chr * cos(hue),
    chr * sin(hue)
  )

  dimnames(mat) <- NULL

  mat
}

#' @rdname pth_to_cartesian
#' @export
#'
pth_to_polar <- function(mat_cartesian) {

  assertthat::assert_that(
    is_mat(mat_cartesian)
  )

  lum <- mat_cartesian[, 1]
  a <- mat_cartesian[, 2]
  b <- mat_cartesian[, 3]

  # new columns are lum, chroma, hue
  mat <- cbind(
    lum,
    sqrt(a^2 + b^2),
    to_degrees(atan2(b, a)),
    deparse.level = 0 # ignore labels
  )

  dimnames(mat) <- NULL

  mat
}

to_radians <- function(deg) {
  deg * pi / 180
}

to_degrees <- function(rad) {
  (rad * 180 / pi + 360) %% 360
}

# is numeric, is matrix, and has three columns
is_mat <- function(mat) {
  is.numeric(mat) && is.matrix(mat) && identical(ncol(mat), 3L)
}

