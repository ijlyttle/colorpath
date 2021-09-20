#' Create rescaler function
#'
#' @description
#' A rescaler function is used to rescale the input to a palette function.
#'
#' \describe{
#'   \item{`pth_rescaler_reverse()`}{Simply reverses the input.}
#'   \item{`pth_rescaler_domain()`}{Expands the input `domain` to extend from
#'     zero to one.}
#'   \item{`pth_rescaler_euclid()`}{Uses `palette` and `pth_distance_euclid()`
#'     to rescale the input so that the distance-change is constant.}
#'   \item{`pth_rescaler_metric()`}{Uses `palette` and `pth_distance_metric()`
#'     to rescale the input so that the distance-change is constant.}
#' }
#'
#' @inheritParams pth_palette_rescale_reverse
#' @param domain `numeric` vector with length 2, each value between 0 and 1.
#' Rescales to map new inputs of 0 and 1 to these values.
#'
#' @return `function`, for each input (`0 <= x <= 1`) returns a
#'   value (`0 <= y <= 1`). The function shall be monotonic.
#'
#' @keywords internal
#' @export
#'
pth_rescaler_reverse <- function() {
  function(x) {
    1 - x
  }
}

#' @rdname pth_rescaler_reverse
#' @export
#'
pth_rescaler_domain <- function(domain = c(0, 1)) {
  assertthat::assert_that(
    is.numeric(domain),
    identical(length(domain), 2L),
    domain[1] >= 0,
    domain[1] <= 1,
    domain[2] >= 0,
    domain[2] <= 1
  )

  function(x) {
    domain[1] + x * (domain[2] - domain[1])
  }
}

#' @rdname pth_rescaler_reverse
#' @export
#'
pth_rescaler_euclid <- function(palette, tolerance, non_luminance_weight,
                                transformer, ...) {

  assertthat::assert_that(
    inherits(palette, "pth_palette"),
    assertthat::is.number(tolerance),
    assertthat::is.number(non_luminance_weight),
    is.function(transformer)
  )

  # create distance function: given n, returns vector of n+1 distances
  f_distance <- function(n) {

    x <- seq(0, 1, length.out = n + 1)

    color <- palette(x)

    pth_distance_euclid(
      color,
      transformer = transformer,
      non_luminance_weight = non_luminance_weight,
      ...
    )
  }

  # get converged distance-intervals
  intervals <- get_distance_intervals(f_distance, tolerance)

  # get approximation function
  rescaler_distance(intervals)
}

#' @rdname pth_rescaler_reverse
#' @export
#'
pth_rescaler_metric <- function(palette, tolerance,
                                method = c("cie2000", "cie94", "cie1976", "cmc")) {

  assertthat::assert_that(
    inherits(palette, "pth_palette"),
    assertthat::is.number(tolerance)
  )

  method <- match.arg(method)

  # create distance function: given n, return vector of n+1 distances
  f_distance <- function(n) {

    x <- seq(0, 1, length.out = n + 1)

    color <- palette(x)

    pth_distance_metric(color, method = method)
  }

  # get converged distance-intervals
  intervals <- get_distance_intervals(f_distance, tolerance)

  # use approximation function
  rescaler_distance(intervals)
}

# given function for distance and tolerance, return input-rescaler function
rescaler_distance <- function(intervals) {

  distance_scaled <- c(0, cumsum(intervals) / sum(intervals))
  input_scaled <- seq(0, 1, length.out = length(distance_scaled))

  stats::approxfun(distance_scaled, input_scaled)
}

# given function for distance and tolerance, converged vector of distances
# f_distance: given n, return vector of n distance-intervals
get_distance_intervals <- function(f_distance, tolerance) {

  n <- 1
  distance_old <- Inf
  epsilon <- Inf

  while (epsilon > tolerance) {

    # get new distance intervals
    intervals <- f_distance(n)

    distance_new <- sum(intervals)
    epsilon <- abs(1 - distance_new / distance_old)

    distance_old <- distance_new
    n <- 2 * n
  }

  intervals
}
