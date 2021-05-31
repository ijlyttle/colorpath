#' Dataset for surfaces
#'
#' @param x `object` with S3 class `pth_surface`.
#' @param step `numeric` size of step in luminance and chroma.
#' @param ... other args, not used.
#'
#' @return `tibble` with columns `luminance`, `chroma`, `hue`, `hex`.
#'
#' @export
#'
pth_surface_data <- function(x, ...) {
  UseMethod("pth_surface_data")
}

#' @rdname pth_surface_data
#' @export
#'
pth_surface_data.default <- function(x, ...) {
  stop(
    glue::glue("No method for class {class(x)}")
  )
}

#' @rdname pth_surface_data
#' @export
#'
pth_surface_data.pth_surface <- function(x, step = 0.5, ...) {

  mat_sfc <- mat_surface2(x, step = step)

  tibble_lchhex(mat_sfc)
}


#' Create matrix for HL surface
#'
#' @param sfc `function` with S3 class `pth_surface`
#' @param step `numeric` size of step in luminance and chroma
#'
#' @return `matrix` with S3 class `pth_mat` and columns named
#'   `lum`, `chroma`, `hue`
#'
#' @noRd
#'
mat_surface2 <- function(sfc, step) {

  assertthat::assert_that(
    inherits(sfc, "pth_surface"),
    assertthat::is.number(step)
  )

  n_lum <- floor(100 / step)

  # set luminance to center of cells with size approx. equal to step
  lum <- seq(from = step / 2, by = step, length.out = n_lum)
  # chroma max
  chroma <- sfc$fn_max_chroma(lum)
  # hue is a function of luminance
  hue <- sfc$fn_hue(lum)

  # polar matrix using max chroma
  mat_polar <- matrix(c(lum, chroma, hue), ncol = 3, byrow = FALSE)

  # split by rows, into a list of one-row matrices
  list_mat_polar <- asplit(mat_polar, 1L)

  # for each member, build out a larger matrix with a row for each chroma
  list_mat_polar <- purrr::map(list_mat_polar, mat_lch, step = step)

  # combine the matrices
  result_polar <- do.call(rbind, list_mat_polar)

  # convert to cartesian, apply color space
  result_cart <- pth_to_cartesian(result_polar)
  result <- pth_creator(sfc$colors)(result_cart)

  result
}
