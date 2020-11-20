
hex <- pth_new_hex(c("#000000", "#663399", "#ffffff"))
luv <- pth_to_cieluv(hex)
lab <- pth_to_cielab(hex)

test_that("is_color() works", {
  expect_false(
    is_color(3)
  )

  expect_true(
    is_color("#663399")
  )

  expect_true(
    is_color(pth_to_cam02ucs("#663399"))
  )
})

test_that("get_colors() works", {

  hex_one <- head(hex, 1)
  luv_one <- head(luv, 1)

  hex_two <- pth_new_hex(rep(hex_one, 2))
  luv_two <- pth_to_cieluv(hex_two)

  hex_three <- pth_new_hex(rep(hex_one, 3))
  luv_three <- pth_to_cieluv(hex_three)

  # coerce to pth_hex
  expect_identical(
    get_colors(unclass(hex), unclass(hex)),
    list(color_a = hex, color_b = hex)
  )

  # no-op
  expect_identical(
    get_colors(hex_one, hex_one),
    list(color_a = hex_one, color_b = hex_one)
  )

  # we repeat color_b
  expect_identical(
    get_colors(hex, hex_one),
    list(color_a = hex, color_b = hex_three)
  )

  # we stagger color_a
  expect_identical(
    get_colors(hex, NULL),
    list(color_a = head(hex, -1), color_b = tail(hex, -1))
  )

  # no-op
  expect_identical(
    get_colors(hex, hex),
    list(color_a = hex, color_b = hex)
  )

  # error
  expect_error(
    get_colors(hex_two, hex_three),
    "not compatible"
  )

  # pth_mat_cieluv

  # no-op
  expect_identical(
    get_colors(luv_one, luv_one),
    list(color_a = luv_one, color_b = luv_one)
  )

  # we repeat color_b
  expect_identical(
    get_colors(luv, luv_one),
    list(color_a = luv, color_b = luv_three)
  )

  # we stagger color_a
  expect_identical(
    get_colors(luv, NULL),
    list(color_a = head(luv, -1), color_b = tail(luv, -1))
  )

  # no-op
  expect_identical(
    get_colors(luv, luv),
    list(color_a = luv, color_b = luv)
  )

  # error
  expect_error(
    get_colors(luv_two, luv_three),
    "not compatible"
  )

})

test_that("get_transformer_default() works", {

  expect_identical(
    get_transformer_default(hex, hex),
    pth_to_cieluv
  )

  expect_identical(
    get_transformer_default(luv, luv),
    identity
  )

  expect_identical(
    get_transformer_default(luv, lab),
    NULL
  )

})

test_that("distance functions work", {

  # TODO: find some *actual* reference distances, perhaps use snapshot testing

  # single set of colors
  euclid_hex <- pth_distance_euclid(hex)
  metric_hex <- pth_distance_metric(hex)

  # output length is one less than input length
  expect_length(euclid_hex, length(hex) - 1)
  expect_length(metric_hex, length(hex) - 1)

  # all distances are non-negative
  expect_true(all(euclid_hex >= 0))
  expect_true(all(metric_hex >= 0))

  # pairwise comparison with self, distance is zero
  expect_identical(
    pth_distance_euclid(hex, hex),
    rep(0, 3)
  )

  expect_identical(
    pth_distance_metric(hex, hex),
    rep(0, 3)
  )

  # compare with reference
  euclid_hex <- pth_distance_euclid(hex, hex[1])
  metric_hex <- pth_distance_metric(hex, hex[1])

  # output length equals first-input length
  expect_length(euclid_hex, length(hex))
  expect_length(metric_hex, length(hex))

  # all distances are non-negative
  expect_true(all(euclid_hex >= 0))
  expect_true(all(metric_hex >= 0))

  # all distances are non-negative
  expect_true(all(euclid_hex >= 0))
  expect_true(all(metric_hex >= 0))

  # sanity checks:
  #  we expect the distance between black and white to be about 100
  hex_limits <- c("#000000", "#ffffff")

  expect_euclid <- function(transformer) {
    expect_equal(
      pth_distance_euclid(hex_limits, transformer = transformer),
      100,
      tolerance = 3.e-4
    )
  }

  expect_euclid(pth_to_cielab)
  expect_euclid(pth_to_cieluv)
  expect_euclid(pth_to_cam02ucs)
  expect_euclid(pth_to_cam16ucs)
  expect_euclid(pth_to_jzazbz100)

  expect_metric <- function(method, value = 100) {
    expect_equal(
      pth_distance_metric(hex_limits, method = method),
      value,
      tolerance = 1.e-4
    )
  }

  expect_metric("cie2000")
  expect_metric("cie1976")
  expect_metric("cie94")
  expect_metric("cmc", 97.85)

})

