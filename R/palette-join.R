pth_palette_join <- function(palette_low, palette_high, ...) {
  UseMethod("pth_palette_join")
}

pth_palette_join.default <- function(palette_low, palette_high, ...) {
  stop(
    glue::glue("No method for class {class(palette_low,)}")
  )
}

pth_palette_join.pth_palette_hex <- function(palette_low, palette_high, ...) {

  nodes_low  <- attr(palette_low, "nodes")
  nodes_high <- attr(palette_high, "nodes")

  structure(
    palette_join(palette_low, palette_high),
    class = c("pth_palette_hex", "pth_palette"),
    nodes = c(nodes_low, nodes_high)
  )
}

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

    x_low  <- 2 * x[use_low]
    x_high <- 2 * (x[use_high] - 0.5)

    # start with blank matrix
    result <-
      pth_mat_replace_data(
        test_low,
        matrix(rep(0, 3 * length(x)), ncol = 3)
      )

    # substitute low and high values
    result[use_low, ] <- palette_low(x_low)
    result[use_high, ] <- palette_low(x_high)

    result
  }

  f
}
