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

  .get_distance(pal_luv, n, method, ...)
}

.get_distance <- function(pal_luv, n, method, ...) {

  seq_all <- seq(0, 1, by = 1 / floor(n))

  luv <- pal_luv(seq_all)

  luv_from <- utils::head(luv, -1)
  luv_to <- utils::tail(luv, -1)

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

interval_distance <- function(pal_luv, tolerance = 1.e-4, method = "cie2000",
                              ...) {

  assertthat::assert_that(
    inherits(pal_luv, "cpath_pal_luv"),
    assertthat::is.number(tolerance),
    tolerance > 0
  )

  # make this only a function of number of intervals
  interval_n <- function(n) {
    .get_distance(pal_luv, n = n, method = method, ...)
  }

  n <- 1
  distance_old <- Inf
  epsilon <- Inf

  while (epsilon > tolerance) {

    interval <- interval_n(n)

    distance_new <- sum(interval)
    epsilon <- abs(1 - distance_new / distance_old)

    distance_old <- distance_new
    n <- 2 * n
  }

  interval
}

appxfn_distance <- function(interval) {

  dist_scaled <- c(0, cumsum(interval) / sum(interval))
  input_scaled <- seq(0, 1, length.out = length(interval) + 1)

  stats::approxfun(dist_scaled, input_scaled)
}
