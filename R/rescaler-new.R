#' Create rescaler function
#'
#' @description
#' A rescaler function is used to rescale the input to a palette function.
#'
#' \describe{
#'   \item{`pth_rescaler_reverse()`}{Simply reverses the input.}
#'   \item{`pth_rescaler_euclid()`}{Uses `palette` and `pth_distance_euclid()`
#'     to rescale the input so that the distance-change is constant.}
#'   \item{`pth_rescaler_metric()`}{Uses `palette` and `pth_distance_metric()`
#'     to rescale the input so that the distance-change is constant.}
#' }
#'
#' @inheritParams pth_palette_rescale_reverse
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
pth_rescaler_euclid <- function(palette, tolerance, transformer,
                                non_luminance_weight, ...) {

}

#' @rdname pth_rescaler_reverse
#' @export
#'
pth_rescaler_metric <- function(palette, tolerance, method) {

}

