library("magrittr")

luv_space <- colorio$CIELUV(whitepoint = whitepoints_cie1931("D65"))

# this is painful
luv_gamut_test <-
  matrix(
    c(258, 127, 128,
      -4,  20,  20,
      256, 257,   2,
      128, 128, 128,
        0, 128, 128,
      3,  4,   -5),
    ncol = 3,
    byrow = TRUE
  ) %>%
  rgb255_to_xyz100() %>%
  t() %>%
  luv_space$from_xyz100() %>%
  t() %>%
  pth_new_cieluv(whitepoint = whitepoints_cie1931("D65"))

test_that("x_gamut() works", {
  expect_equal(
    x_gamut(c("#000000", "#FF0000", "#FFFF00")),
    c(0, 0, 0)
  )

  expect_equal(
    x_gamut(c("#010203", "#FE0203", "#FEFD02")),
    c(-1, -1, -1)
  )

  expect_equal(
    x_gamut(luv_gamut_test),
    c(3, 4, 2, -127, 0, 5)
  )
})

test_that("pth_in_gamut() works", {
  expect_equal(
    pth_in_gamut(luv_gamut_test),
    c(FALSE, FALSE, FALSE, TRUE, TRUE, FALSE)
  )

  expect_true(
    all(
      pth_in_gamut(c("#663399", "#000000", "#ffffff"))
    )
  )

})
