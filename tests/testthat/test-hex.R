hex_good <- c("#112233", "#AABBCC", "#11223344")
hex_correct <- c("#112233", "#aabbcc", "#112233")
hex_bad  <- "foo"

test_that("is_hex_liberal() works", {
  expect_true(all(is_hex_liberal(hex_good)))
  expect_false(all(is_hex_liberal(hex_bad)))
})

test_that("as_hex() works", {
  expect_identical(as_hex(hex_good), hex_correct)
})

test_that("pth_new_hex() works", {
  expect_identical(pth_new_hex(hex_good), hex_correct)
  expect_error(pth_new_hex(hex_bad), "hex-code")
})

