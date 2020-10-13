#' Get perceptual distance
#'
#' Uses [farver::compare_colour()] to calculate perceptual distances
#' along intervals over a palette-function.
#'
#' @inheritParams data_hcl
#' @param n `numeric` number of intervals into which to split domain
#'   of the palette.
#' @param method `character` method to use for distance, one of
#'   `"cie2000"`, `"euclidean"`, `"cie1976"`, `"cie94"`, or `"cmc"`.
#' @param ... other arguments sent to [farver::compare_colour()].
#'
#' @return `numeric` vector, length `n`, perceptual distances for
#'   each interval.
#'
#' @export
#'
get_distance <- function(pal_luv, n = 20, method = "cie2000", ...) {

  assertthat::assert_that(
    inherits(pal_luv, "cpath_pal_luv"),
    assertthat::is.number(n),
    n > 0
  )

  seq_all <- seq(0, 1, by = 1 / floor(n))

  luv_from <- pal_luv(utils::head(seq_all, -1))
  luv_to <- pal_luv(utils::tail(seq_all, -1))

  # create list, each element is a row of the matrix
  to_list <- function(x) {
    purrr::map(seq(nrow(x)), ~x[.x, , drop = FALSE])
  }

  list_from <- to_list(luv_from)
  list_to <- to_list(luv_to)

  purrr::map2_dbl(
    list_from,
    list_to,
    farver::compare_colour,
    from_space = "luv",
    method = method,
    ...
  )
}
