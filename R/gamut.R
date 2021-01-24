pth_data_gamut <- function(n_point = 5, transformer = pth_to_cieluv) {


}

# returns matrix with r g b
srgb_gamut <- function(n_point, set = c("edge", "vertex", "surface", "all")) {

  assertthat::assert_that(
    assertthat::is.number(n_point),
    n_point >= 0
  )

  set <- match.arg(set)

  n_dim_match <- c(all = 0, surface = 1, edge = 2, vertex = 3)

  vec <- seq(0, 255, length.out = n_point)

  grid_full <- expand.grid(r = vec, g = vec, b = vec)

  # returns number of dimensions at max or min
  n_dim_minmax <- function(x) {
    (x$r == 0 | x$r == 255) +
    (x$g == 0 | x$g == 255) +
    (x$b == 0 | x$b == 255)
  }

  # returns logical
  is_match <- function(x) {
    n_dim_minmax(x) >= n_dim_match[[set]]
  }

  grid_full[is_match(grid_full), ]
}
