
#' Create matrix for HL surface
#'
#' @param sfc `function` with S3 class `pth_surface`
#' @param mat_example matrix with S3 class `pth_mat`, example of
#'   the color space to use
#' @param step `numeric` size of step in luminance and chroma
#'
#' @return `matrix` with S3 class `pth_mat` and columns named
#'   `lum`, `chroma`, `hue`
#'
#' @noRd
#'
mat_surface <- function(sfc, mat_example, step) {

  assertthat::assert_that(
    inherits(sfc, "pth_surface"),
    inherits(mat_example, "pth_mat"),
    assertthat::is.number(step)
  )

  n_lum <- floor(100 / step)

  # set luminance to center of cells with size approx. equal to step
  lum <- seq(from = step / 2, by = step, length.out = n_lum)
  # chroma zero
  chroma <- rep(0, n_lum)
  # hue is a function of luminance
  hue <- sfc(lum)

  # start with a matrix with zero chroma
  mat_polar <- matrix(c(lum, chroma, hue), ncol = 3, byrow = FALSE)

  # convert to cartesian, apply color space
  mat_cart <- pth_to_cartesian(mat_polar)
  mat_cart <- pth_mat_replace_data(mat_example, mat_cart)

  # set chroma on polar matrix to max chroma
  mat_polar[, 2] <- pth_max_chroma(mat_cart)

  # split by rows, into a list of one-row matrices
  list_mat_polar <- asplit(mat_polar, 1L)

  # for each member, build out a larger matrix with a row for each chroma
  list_mat_polar <- purrr::map(list_mat_polar, mat_lch, step = step)

  # combine the matrices
  result_polar <- do.call(rbind, list_mat_polar)

  # convert to cartesian, apply color space
  result_cart <- pth_to_cartesian(result_polar)
  result <- pth_mat_replace_data(mat_example, result_cart)

  result
}

#' Create LCH matrix along chroma
#'
#' @param mat `numeric` one-row matrix
#' @param step `numeric` step in chroma
#'
#' @return `matrix` with S3 class `pth_mat` and columns named
#'   `lum`, `chroma`, `hue`
#'
#' @noRd
#'
mat_lch <- function(mat, step) {

  # decode matrix
  lum <- mat[1]
  max_chroma <- mat[2]
  hue <- mat[3]

  n_chroma <- floor(max_chroma / step)

  # what if there is nothing to send back?
  if (identical(n_chroma, 0L)) {
    return(NULL)
  }

  # single hue
  hue <- rep(hue, n_chroma)
  # set chroma to center of cells with size equal to step
  chroma <- seq(from = step / 2, by = step, length.out = n_chroma)
  # single luminance
  lum <- rep(lum, n_chroma)

  matrix(c(lum, chroma, hue), ncol = 3, byrow = FALSE)
}


tibble_surface <- function(sfc, example, step) {

  # get cartesian surface
  mat_cart <- mat_surface(sfc, example, step)

  tibble_lchhex(mat_cart)
}

tibble_lchhex <- function(mat_cart) {

  # convert to polar
  mat_polar <- pth_to_polar(mat_cart)

  # return tibble
  tibble::tibble(
    luminance = round(mat_polar[, 1], 6),
    chroma = round(mat_polar[, 2], 6),
    hue = round(mat_polar[, 3], 6),
    hex = pth_to_hex(mat_cart)
  )
}

#' Dataset for trajectories
#'
#' @param x `function` with S3 class `pth_trajectory` or
#'  `pth_palette_path`.
#' @param ... other arguments (not used).
#'
#' @return `tibble` with columns `luminance`, `chroma`
#'
#' @export
#'
pth_data_control_points <- function(x, ...) {
  UseMethod("pth_data_control_points")
}

#' @rdname pth_data_control_points
#' @export
#'
pth_data_control_points.default <- function(x, ...) {
  stop(
    glue::glue("No method for class {class(x)}")
  )
}

#' @rdname pth_data_control_points
#' @export
#'
pth_data_control_points.pth_trajectory <- function(x, ...) {

  mat <- attr(x, "control_points")
  tibble_control_points(mat)
}

#' @rdname pth_data_control_points
#' @export
#'
pth_data_control_points.pth_palette_path <- function(x, ...) {

  list_mat <- attr(x, "control_points")
  list_df <- purrr::map(list_mat, tibble_control_points)

  # diverging
  if (length(list_df) > 1) {
    # negate the chroma for the first set
    list_df[[1]][["chroma"]] <- -list_df[[1]][["chroma"]]
  }

  do.call(rbind, list_df)
}

tibble_control_points <- function(mat) {
  tibble::tibble(
    luminance = mat[, 1],
    chroma = mat[, 2]
  )
}

#' Dataset for palette
#'
#' @param palette `function` with S3 class `pth_palette`
#' @param n `numeric` number of colors.
#' @param ... other arguments (not used).
#'
#' @return `tibble` with columns `luminance`, `chroma`, `hue`, `hex`
#'
#' @export
#'
pth_data_palette <- function(palette, ...) {
  UseMethod("pth_data_palette")
}

#' @rdname pth_data_palette
#' @export
#'
pth_data_palette.default <- function(palette, ...) {
  stop(
    glue::glue("No method for class {class(palette)}")
  )
}

#' @rdname pth_data_palette
#' @export
#'
pth_data_palette.pth_palette <- function(palette, n = 10, ...) {

  x <- seq(0, 1, length.out = n)
  mat_cart <- palette(x)

  mat_polar <- tibble_lchhex(mat_cart)

  if (is_diverging(palette)) {
    first_half <- seq(1, floor(n / 2))
    mat_polar$chroma[first_half] <- -mat_polar$chroma[first_half]
  }

  mat_polar
}

is_diverging <- function(palette) {

  # uses luminance heuristic

  x <- c(0, 0.5, 1)
  lum <- palette(x)[, 1]

  # does direction of luminance change?
  sign(lum[2] - lum[1]) != sign(lum[3] - lum[2])
}


