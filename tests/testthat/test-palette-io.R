
hex_blue <- c("#e2e2e2", "#9cbaee", "#3c79c0")

pal_blue <- pth_new_palette_hex(hex_blue)
pal_blue_discrete <- pth_pal_input_discrete(pal_blue)
pal_blue_hex <- pth_pal_output_hex(pal_blue)

n <- 5
x <- seq(0, 1, length.out = n)

test_that("pth_pal_input_discrete() works", {

  expect_error(
    pth_pal_input_discrete("foo"),
    "function"
  )

  expect_type(pal_blue_discrete, "closure")

  expect_identical(pal_blue_discrete(n), pal_blue(x))

})

test_that("pth_pal_output_hex() works", {

  expect_error(
    pth_pal_output_hex("foo"),
    "function"
  )

  expect_type(pal_blue_hex, "closure")

  expect_identical(
    pal_blue_hex(x),
    pal_blue(x) %>% pth_to_hex()
  )

})
