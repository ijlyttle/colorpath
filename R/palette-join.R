#' Join palette functions
#'
#' Use this function to join two sequential palettes into a diverging palette.
#'
#' @param palette_low,palette_high `function` with S3 class `pth_palette`,
#'   palettes to join. `palette_low` will be reversed; both palettes will be
#'   rescaled then joined.
#' @param ... other args (not used)
#'
#' @return `function` with S3 class `pth_palette`,
#'   same as `palette_low`, `palette_high`.
#' @examples
#'  # both these palettes go from light grey to a darker color
#'  pal_hex_blue <- pth_new_palette_hex(c("#e2e2e2", "#9cbaee", "#3c79c0"))
#'  pal_hex_orange <- pth_new_palette_hex(c("#e2e2e2", "#e0af85", "#a66a00"))
#'
#'  # palette_low (in this case, blue) is reversed,
#'  # putting the light grey in the middle
#'  pal_hex_join <- pth_palette_join(pal_hex_blue, pal_hex_orange)
#'
#'  # recover the original hex codes
#'  pal_hex_join(seq(0, 1, by = 0.25)) %>% pth_to_hex()
#'
#' @export
#'
pth_palette_join <- function(palette_low, palette_high, ...) {
  UseMethod("pth_palette_join")
}

#' @rdname pth_palette_join
#' @export
#'
pth_palette_join.default <- function(palette_low, palette_high, ...) {
  stop(
    glue::glue("No method for class {class(palette_low)}")
  )
}

#' @rdname pth_palette_join
#' @export
#'
pth_palette_join.pth_palette_hex <- function(palette_low, palette_high, ...) {

  nodes_low  <- attr(palette_low, "nodes")
  nodes_high <- attr(palette_high, "nodes")

  structure(
    palette_join(palette_low, palette_high),
    class = c("pth_palette_hex", "pth_palette"),
    nodes = c(nodes_low, nodes_high)
  )
}

#' @rdname pth_palette_join
#' @export
#'
pth_palette_join.pth_palette_path <- function(palette_low, palette_high, ...) {

  cp_low  <- attr(palette_low, "control_points")
  cp_high <- attr(palette_high, "control_points")

  surf_low  <- attr(palette_low, "surface")
  surf_high <- attr(palette_high, "surface")

  structure(
    palette_join(palette_low, palette_high),
    class = c("pth_palette_hex", "pth_palette"),
    control_points = c(cp_low, cp_high),
    surface = c(surf_low, surf_high)
  )
}

palette_join <- function(palette_low, palette_high) {

  assertthat::assert_that(
    identical(class(palette_low), class(palette_high))
  )

  # check that these palettes use the same color space and attributes
  test_low <- palette_low(0)
  test_high <- palette_high(0)
  assertthat::assert_that(
    identical(attributes(test_low), attributes(test_high))
  )

  # TODO: some clever code to match the two palettes automatically

  f <- function(x) {

    use_low <- (x <= 0.5)
    use_high <- (x > 0.5)

    # reverse the low palette
    x_low  <- 2 * (0.5 - x[use_low])
    x_high <- 2 * (x[use_high] - 0.5)

    # start with blank matrix
    result <-
      pth_mat_replace_data(
        test_low,
        matrix(rep(0, 3 * length(x)), ncol = 3)
      )

    # substitute low and high values
    if (any(use_low)) {
      result[use_low, ] <- palette_low(x_low)
    }

    if (any(use_high)) {
      result[use_high, ] <- palette_high(x_high)
    }

    result
  }

  f
}
