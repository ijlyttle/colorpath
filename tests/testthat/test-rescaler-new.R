# not perceptually uniform
pal_grey <- pth_new_palette_hex(c("#000000", "#666666", "#ffffff"))

test_that("pth_rescaler_reverse() works", {

  r <- pth_rescaler_reverse()
  x <- seq(0, 1, length.out = 10)

  expect_type(r, "closure")
  expect_identical(r(x), 1 - x)

})

test_that("pth_rescaler_domain() works", {

  # test validation
  expect_error(pth_rescaler_domain("a"))
  expect_error(pth_rescaler_domain(c(0, 1, 0.5)))
  expect_error(pth_rescaler_domain(c(-0.5, 1)))
  expect_error(pth_rescaler_domain(c(1, -0.5)))

  r <- pth_rescaler_domain(domain = c(0.25, 0.75))
  x <- seq(0, 1, length.out = 10)

  expect_type(r, "closure")

  expect_identical(r(x), 0.25 + 0.5 * x)

})

test_that("pth_rescaler_euclid()", {

  expect_error(
    pth_rescaler_euclid(
      palette = "foo",
      tolerance = 1.e-4,
      non_luminance_weight = 1,
      transformer = identity
    ),
    "pth_palette"
  )

  expect_error(
    pth_rescaler_euclid(
      palette = pal_grey,
      tolerance = "foo",
      non_luminance_weight = 1,
      transformer = identity
    ),
    "tolerance"
  )

  expect_error(
    pth_rescaler_euclid(
      palette = pal_grey,
      tolerance = 1.e-4,
      non_luminance_weight = "foo",
      transformer = identity
    ),
    "non_luminance_weight"
  )

  expect_error(
    pth_rescaler_euclid(
      palette = pal_grey,
      tolerance = 1.e-4,
      non_luminance_weight = 1,
      transformer = "foo"
    ),
    "transformer"
  )

  rsc_grey <- pth_rescaler_euclid(
    palette = pal_grey,
    tolerance = 1.e-4,
    non_luminance_weight = 1,
    transformer = identity
  )

  # endpoints are equal
  expect_equal(
    rsc_grey(c(0, 1)),
    c(0, 1)
  )

  # midpoint has to aim a little higher than midpoint in original palette
  # midpoint should be #777777
  expect_gt(rsc_grey(0.5), 0.5)

  # monotonic
  rsc_grey_seq <- rsc_grey(seq(0, 1, by = 0.05))
  diff <- tail(rsc_grey_seq, -1) - head(rsc_grey_seq, -1)
  expect_true(
    all(diff > 0)
  )
})

test_that("pth_rescaler_metric()", {

  expect_error(
    pth_rescaler_metric(
      palette = "foo",
      tolerance = 1.e-4,
      method = "cie2000"
    ),
    "pth_palette"
  )

  expect_error(
    pth_rescaler_metric(
      palette = pal_grey,
      tolerance = "foo",
      method = "cie2000"
    ),
    "tolerance"
  )

  expect_error(
    pth_rescaler_metric(
      palette = pal_grey,
      tolerance = 1.e-4,
      method = "foo"
    ),
    "arg"
  )

  rsc_grey <- pth_rescaler_metric(
    palette = pal_grey,
    tolerance = 1.e-4,
    method = "cie2000"
  )

  # endpoints are equal
  expect_equal(
    rsc_grey(c(0, 1)),
    c(0, 1)
  )

  # midpoint has to aim a little higher than midpoint in original palette
  # midpoint should be #777777
  expect_gt(rsc_grey(0.5), 0.5)

  # monotonic
  rsc_grey_seq <- rsc_grey(seq(0, 1, by = 0.05))
  diff <- tail(rsc_grey_seq, -1) - head(rsc_grey_seq, -1)
  expect_true(
    all(diff > 0)
  )
})

test_that("get_distance_intervals() and rescaler_distance() work", {

  # distance from 0 to 1 is 1/3
  f_one_third <- function(n) {

    x <- seq(0, 1, length.out = n + 1)
    y <- x^2

    dy <- (tail(y, -1) + head(y, -1)) / (2 * n)

    dy
  }

  # get the intervals that integrate x^2 from 0 to 1, get enough
  # so that the answer converges within the tolerance
  intervals <- get_distance_intervals(f_one_third, 1.e-4)

  # expect that the integral is 1/3
  expect_equal(sum(intervals), 1 / 3, tolerance = 1.e-4)

  # rescale the input so that a constant change in x
  # results in a constant change in y
  rescaler <- rescaler_distance(intervals)

  # in this case, we expect tthe rescaler to take the cube root of x
  x <- seq(0, 1, by = 0.1)
  x_ref <- x^(1/3)

  expect_equal(rescaler(x), x_ref, tolerance = 1.e-4)

})
