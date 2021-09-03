#' Color-vision deficiency grid
#'
#' Helper function to generate grid for color-vision deficiency (CVD)
#' possibilities.
#'
#' @param condition `character` one or more color-vision deficiency conditions;
#'  legal values: `"none"`, `"deutan"`, `"protan"`, `"tritan"`.
#' @param severity `double` one or more values of severity for a given
#'  condition; legal values from 0 to 1.
#'
#' @return `tbl_df` with columns `cvd` (factor), `severity` (double)
#' @examples
#'   pth_cvd_grid_none()
#'   pth_cvd_grid_full()
#'   pth_cvd_grid_severity(0.9)
#' @export
#'
pth_cvd_grid <- function(condition = c("none", "deutan", "protan", "tritan"),
                         severity = 1) {

  # validate inputs
  condition <- match.arg(condition, several.ok = TRUE)

  assertthat::assert_that(
    is.numeric(severity),
    all(severity >= 0),
    all(severity <= 1)
  )

  expand.grid(condition = condition, severity = severity) %>%
    tibble::as_tibble()
}

#' @rdname pth_cvd_grid
#' @export
#'
pth_cvd_grid_full <- function(condition = c("deutan", "protan", "tritan"),
                              severity = c(0, 0.33, 0.67, 1)) {

  pth_cvd_grid(condition = condition, severity = severity)
}

#' @rdname pth_cvd_grid
#' @export
#'
pth_cvd_grid_none <- function(condition = "none", severity = 0) {

  pth_cvd_grid(condition = condition, severity = severity)
}

#' @rdname pth_cvd_grid
#' @export
#'
pth_cvd_grid_severity <- function(severity = 1) {

  result <-
    rbind(
      pth_cvd_grid_none(),
      pth_cvd_grid_full(severity = severity)
    )


  tibble::as_tibble(result)
}

#' Color data in CVD context
#'
#' Generate versions of colors using color-vision deficiency simulation.
#'
#' @inheritParams pth_distance_euclid
#' @param x `character`, `pth_mat`, or `pth_palette`: a means to express color
#' @param cvd `tbl_df` with columns `condition`, `severity`;
#'   see [pth_cvd_grid()].
#' @param n_point `integer` number of points from the palette.
#' @param ... other args, not used.
#'
#' @return `tibble` with columns `"hex_original"`, `"condition"`, `"severity"`,
#'   `"luminance"`, `"chroma"`, `"hue"`, `"hex"`.
#'
#' @export
#'
pth_data_cvd <- function(x, cvd = pth_cvd_grid(), ...) {
  UseMethod("pth_data_cvd")
}

#' @rdname pth_data_cvd
#' @export
#'
pth_data_cvd.default <- function(x, cvd = pth_cvd_grid(), ...) {
  stop(
    glue::glue("No method for class {class(x)}")
  )
}

#' @rdname pth_data_cvd
#' @export
#'
pth_data_cvd.character <- function(x, cvd = pth_cvd_grid(),
                                   transformer = pth_to_cieluv, ...) {

  # get data in pth_mat form
  pth_mat <- transformer(x)

  pth_data_cvd(pth_mat, cvd = cvd, ...)
}

#' @rdname pth_data_cvd
#' @export
#'
pth_data_cvd.pth_palette <- function(x, cvd = pth_cvd_grid(),
                                     n_point = 11, ...) {
  # get data in pth_mat form
  pth_mat <- x(seq(0, 1, length.out = n_point))

  pth_data_cvd(pth_mat, cvd = cvd, ...)
}

#' @rdname pth_data_cvd
#' @export
#'
pth_data_cvd.pth_mat <- function(x, cvd = pth_cvd_grid(), ...) {

  # validate cvd
  assertthat::assert_that(
    is.data.frame(cvd),
    assertthat::has_name(cvd, c("condition", "severity"))
  )

  together <-
    tibble::tibble(
      condition = as.character(cvd$condition),
      severity = cvd$severity,
      hex_original = list(pth_to_hex(x)), # uses a list so that it's recycled
      mat = list(x) # wraps matrix up in a list so that it's recycled
    )

  together$new <- purrr::pmap(together, mat_cvd)
  together$data <- purrr::map(together$new, tibble_lchhex)

  together$mat <- NULL
  together$new <- NULL

  result <- tidyr::unnest(together, cols = c("data", "hex_original"))

  # make condition a factor
  result$condition <-
    factor(
      result$condition,
      levels = c("none", "deutan", "protan", "tritan")
    )

  result
}

# this could be the start of a way to modify a palette function to
# give its output in CVD.
mat_cvd <- function(mat, condition, severity, ...) {

  # function to put the output into the same color space as the input
  transformer <- pth_transformer(mat)

  mat_cvd <-
    mat %>%
    to_rgb() %>%
    rgb_cvd(condition, severity) %>%
    transformer()

  mat_cvd
}

rgb_cvd <- function(mat_rgb,
                    condition =  c("none", "deutan", "protan", "tritan"),
                    severity) {
  # inouts
  condition <- match.arg(condition)

  assertthat::assert_that(
    assertthat::is.number(severity),
    severity >= 0,
    severity <= 1
  )

  # if condition is "none", no-op
  if (identical(condition, "none")) {
    return(pth_new_srgb255(mat_rgb))
  }

  # get cvd transformation matrix
  transform_cvd_list <-
    list(
      deutan = colorspace::deutanomaly_cvd,
      protan = colorspace::protanomaly_cvd,
      tritan = colorspace::tritanomaly_cvd
    )

  transform_cvd <- transform_cvd_list[[condition]]
  mat_cvd <- colorspace::interpolate_cvd_transform(transform_cvd, severity)

  # calculate new RGB
  result <- t(mat_cvd %*% t(mat_rgb))

  # clamp values
  result[result < 0] <- 0
  result[result > 255] <- 255

  # add names
  dimnames(result) <- list(NULL, c("r", "g", "b"))

  pth_new_srgb255(result)
}
