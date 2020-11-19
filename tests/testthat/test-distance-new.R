
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
