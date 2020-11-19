#' Determine distance colors
#'
#' - if `color_b` is `NULL`, `color_a` is the all but last color of `color_a`;
#'     `color_b` is all but first color of `color_a`.
#' - if `color_b` has length `1`, it is repeated to have the length of `color_a`.
#' - check that `color_b` has the same length as `color_a`, throw error otherwise.
#'
#' @param color_a, color_b object with class `pth_mat` or `pth_hex`
#'
#' @return `list()` with elements `color_a`, `color_b`
#'
#' @noRd
#'
get_colors <- function(color_a, color_b) {

  assertthat::assert_that(
    is_color(color_a),
    is_color(color_b)
  )

  if (is.null(color_b)) {
    colors <- color_a
    color_a <- utils::head(colors, -1)
    color_b <- utils::tail(colors, -1)
  }

  if (identical(pth_n_color(color_b), 1L)) {

    len_a <- pth_n_color(color_a)

    if (is.character(color_b)) {
      new_hex <- rep(color_b, len_a)
      color_b <- pth_new_hex(new_hex)
    }

    if (inherits(color_b, "pth_mat")) {
      new_mat <- do.call(rbind, rep(list(color_b), len_a))
      color_b <- pth_mat_replace_data(color_b, new_mat)
    }
  }

  assertthat::assert_that(
    pth_n_color(color_a) == pth_n_color(color_b),
    msg = "length of `color_a` and `color_b` not compatible"
  )

  # result
  list(color_a = color_a, color_b =  color_b)
}

# operates on the entire structure, not vectorized
is_color <- function(x) {
  is_mat(x) || all(is_hex_liberal(x))
}


#' Get default transformer-function
#'
#' @inheritParams get_colors
#'
#' Call this *after* get_colors() as it will take care of validating and
#' coercing inputs.
#'
#' The point here is to return a default transformation-function
#'
#' - if `color_a` and `color_b` are both hex-codes, we use `pth_to_cieluv()`.
#' - if `color_a` and `color_b` both use the same color space, we use `identity()`.
#' - if `color_a` and `color_b` use different color spaces, return `NULL`.
#'
#' The calling function will have to throw an error if the transformer function
#' ends up `NULL`.
#'
#' @return transformer `function`
#'
#' @noRd
#'
get_transformer_default <- function(color_a, color_b) {

  if (!identical(class(color_a), class(color_b))) {
    return(NULL)
  }

  if (inherits(color_a, "pth_hex")) {
    return(pth_to_cieluv)
  }

  if (inherits(color_a, "pth_mat"))  {
    return(identity)
  }

  NULL
}
