#' Get color distances within palette
#'
#' You can evaluate distance for categorial palettes (`_cat` functions), or for
#' quantitative functions (`_qnt` functions). You can evaluate using Euclidean
#' distance in a color space (`_euclid` functions), or using a metic
#' (`_metric` functions).
#'
#' @inheritParams pth_distance_euclid
#' @param color Object that can be coerced into colors,
#'   i.e. `pth_hex` or `pth_mat`. Used for categorical palettes.
#' @param palette `function` with S3 class `pth_palette`. Used for quantitative
#'   palettes.
#'
#' @return `tbl_df` with columns:
#' \describe{
#'   \item{hex_original_a}{`character` hex-code of original color.}
#'   \item{hex_original_b}{`character` hex-code of original color.}
#'   \item{cvd}{`character` type of color-vision deficiency.}
#'   \item{severity}{`numeric` indicates severity using scale 0-1.}
#'   \item{hex_a}{`character` hex-code of color under CVD.}
#'   \item{hex_b}{`character` hex-code of color under CVD.}
#'   \item{distance}{`numeric` perceptual distance between colors}
#' }
#' Each row describes an interaction between two colors in a palette.
#' For a categorical palette  (`_cat` functions), each color in the palette is
#' compared with all the colors in the palette; for `n` colors, a tibble
#' with `n^2` rows is returned.
#' For a quantitative palette (`_qnt` functions), the palette is discretized;
#' each color is compared with its neighbors. A data frame with `n_step` rows is
#' returned.
#'
#' @export
#'
pth_data_cat_euclid <- function(color, ...) {
  UseMethod("pth_data_cat_euclid")
}

#' @export
#'
pth_data_cat_euclid.default <- function(color, ...) {
  stop(
    glue::glue("No method for class {class(color)}")
  )
}

#' @export
#'
pth_data_cat_euclid.character <- function(color,
                                          cvd = pth_cvd_grid_severity(),
                                          transformer = NULL,
                                          non_luminance_weight = 1, ...) {
  # convert to hex
  color <- pth_hex(color)

  pth_data_cat_euclid(color, cvd, transformer, non_luminance_weight, ...)
}

#' @export
#'
pth_data_cat_euclid.pth_hex <- function(color,
                                        cvd = pth_cvd_grid_severity(),
                                        transformer = NULL,
                                        non_luminance_weight = 1, ...) {

  # convert to matrix
  transformer <- transformer %||% pth_to_cieluv
  color <- transformer(color)

  pth_data_cat_euclid(
    color,
    cvd,
    transformer = identity,
    non_luminance_weight,
    ...
  )
}

#' @export
#'
pth_data_cat_euclid.pth_mat <- function(color,
                                        cvd = pth_cvd_grid_severity(),
                                        transformer = NULL,
                                        non_luminance_weight = 1, ...) {

  # make a list from color-matrix: one element per row
  list_mat <- list_mat(color)

  # cross with itself and cvd specification
  data_all <- color_cvd_cat(list_mat, cvd)

  # get rest of the columns
  data_all <- data_fill(data_all)

  # calculate distance
  data_all[["distance"]] <-
    purrr::pmap_dbl(
      data_all[, c("color_a", "color_b")],
      pth_distance_euclid,
      transformer = transformer,
      non_luminance_weight = non_luminance_weight
    )

  # select columns
  data_all <- data_select(data_all)

  data_all
}

#' @rdname pth_data_cat_euclid
#' @export
#'
pth_data_cat_metric <- function(color, ...) {
  UseMethod("pth_data_cat_metric")
}

#' @export
#'
pth_data_cat_metric.default <- function(color, ...) {
  stop(
    glue::glue("No method for class {class(color)}")
  )
}

#' @export
#'
pth_data_cat_metric.character <- function(color,
                                          cvd = pth_cvd_grid_severity(),
                                          method = c("cie2000", "cie94", "cie1976", "cmc"),
                                          ...) {
  # convert to hex
  color <- pth_hex(color)

  pth_data_cat_metric(color, cvd, method, ...)
}

#' @export
#'
pth_data_cat_metric.pth_hex <- function(color,
                                        cvd = pth_cvd_grid_severity(),
                                        method = c("cie2000", "cie94", "cie1976", "cmc"),
                                        ...) {

  # might be simplest to convert to cieluv, then implment in the pth_mat method
  color <- pth_to_cieluv(color)

  pth_data_cat_metric(color, cvd, method, ...)
}

#' @export
#'
pth_data_cat_metric.pth_mat <- function(color,
                                        cvd = pth_cvd_grid_severity(),
                                        method = c("cie2000", "cie94", "cie1976", "cmc"),
                                        ...) {

  # make a list from color-matrix: one element per row
  list_mat <- list_mat(color)

  # cross with itself and cvd specification
  data_all <- color_cvd_cat(list_mat, cvd)

  # get rest of the columns
  data_all <- data_fill(data_all)

  # calculate distance
  data_all[["distance"]] <-
    purrr::pmap_dbl(
      data_all[, c("color_a", "color_b")],
      pth_distance_metric,
      method = method
    )

  # select columns
  data_all <- data_select(data_all)

  data_all
}

#' @rdname pth_data_cat_euclid
#' @export
#'
pth_data_qnt_euclid <- function(palette, ...) {
  UseMethod("pth_data_qnt_euclid")
}

#' @export
#'
pth_data_qnt_euclid.default <- function(palette, ...) {
  stop(
    glue::glue("No method for class {class(color)}")
  )
}

#' @export
#'
pth_data_qnt_euclid.pth_palette <- function(palette, n_step = 12,
                                            cvd = pth_cvd_grid_severity(),
                                            transformer= NULL,
                                            non_luminance_weight = 1, ...) {

  # discretize palette into colors
  color <- palette(seq(0, 1, length.out = n_step))

  # make a list from color-matrix: one element per row
  list_mat <- list_mat(color)

  # cross with cvd specification
  data_all <- color_cvd_qnt(list_mat, cvd)

  # get rest of the columns
  data_all <- data_fill(data_all)

  # calculate distance
  data_all[["distance"]] <-
    purrr::pmap_dbl(
      data_all[, c("color_a", "color_b")],
      pth_distance_euclid,
      transformer = transformer,
      non_luminance_weight = non_luminance_weight
    )

  # select columns
  data_all <- data_select(data_all)

  data_all
}

#' @rdname pth_data_cat_euclid
#' @export
#'
pth_data_qnt_metric <- function(palette, ...) {
  UseMethod("pth_data_qnt_metric")
}

#' @export
#'
pth_data_qnt_metric.default <- function(palette, ...) {
  stop(
    glue::glue("No method for class {class(color)}")
  )
}

#' @export
#'
pth_data_qnt_metric.pth_palette <- function(palette, n_step = 12,
                                            cvd = pth_cvd_grid_severity(),
                                            method =  c("cie2000", "cie94", "cie1976", "cmc"),
                                            ...) {

  # discretize palette into colors
  color <- palette(seq(0, 1, length.out = n_step))

  # make a list from color-matrix: one element per row
  list_mat <- list_mat(color)

  # cross with cvd specification
  data_all <- color_cvd_qnt(list_mat, cvd)

  # get rest of the columns
  data_all <- data_fill(data_all)

  # calculate distance
  data_all[["distance"]] <-
    purrr::pmap_dbl(
      data_all[, c("color_a", "color_b")],
      pth_distance_metric,
      method = method
    )

  # select columns
  data_all <- data_select(data_all)

  data_all
}

# create list of one-row matrices from matrix
list_mat <- function(mat) {

  creator <- pth_creator(mat) # get creator-function

  list_mat <- asplit(mat, 1) # create list of rows
  list_mat <- purrr::map(list_mat, matrix, ncol = 3) # each element matrix
  list_mat <- purrr::map(list_mat, creator) # each element pth_mat

  list_mat
}

# create data-frame with color_original_a, color_original_b, cvd
#  - categorical palettes: each color is compared with each other color
#    for each row of the cvd data-frame.
color_cvd_cat <- function(list_mat, cvd) {
  data_all <-
    tidyr::crossing(
      color_original_a = list_mat,
      color_original_b = list_mat,
      cvd
    )

  data_all
}

# create data-frame with color_original_a, color_original_b, cvd
#  - quantitative palettes: each color is compared with its neighbor
#    for each row of the cvd data-frame.
color_cvd_qnt<- function(list_mat, cvd) {

  n <- length(list_mat)

  data_all <-
    tidyr::crossing(
      tibble::tibble(
        color_original_a = list_mat[1:(n-1)],
        color_original_b = list_mat[2:n]
      ),
      cvd
    )

  data_all
}


# given a data frame with columns:
#  - color_original_a, color_original_b, condition, severity
# add additional columns:
#  - hex_original_a, hex_original_b, color_a, color_b, hex_a, hex_b
#
data_fill <- function(.data) {

  # hex_original
  .data[["hex_original_a"]] <-
    purrr::map_chr(.data[["color_original_a"]], pth_to_hex)

  .data[["hex_original_b"]] <-
    purrr::map_chr(.data[["color_original_b"]], pth_to_hex)

  # color
  f_cvd_a <- function(color_original_a, condition, severity, ...) {
    mat_cvd(color_original_a, as.character(condition), severity, ...)
  }

  .data[["color_a"]] <-
    purrr::pmap(.data, f_cvd_a)

  f_cvd_b <- function(color_original_b, condition, severity, ...) {
    mat_cvd(color_original_b, as.character(condition), severity, ...)
  }

  .data[["color_b"]] <-
    purrr::pmap(.data, f_cvd_b)

  # hex
  .data[["hex_a"]] <- purrr::map_chr(.data[["color_a"]], pth_to_hex)
  .data[["hex_b"]] <- purrr::map_chr(.data[["color_b"]], pth_to_hex)

  .data
}

# standardize the columns
data_select <- function(.data) {
  cols <- c("hex_original_a", "hex_original_b", "condition", "severity",
            "hex_a", "hex_b", "distance")

  .data[, cols]
}
