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

test_that("pth_max_chroma() works", {

  # all of these are at the gamut outer-surface
  hex <- c("#820076", "#ff8053", "#45a4ff")

  expect_match_chroma <- function(hex, .f) {

    mat <- .f(hex)
    polar <- pth_to_polar(mat)
    max_chroma <- polar[, 2]

    # generate a guess that uses half the chroma
    mat[, 2:3] <- mat[, 2:3] / 2

    # try to recover original
    max_chroma_calc <- pth_max_chroma(mat)

    expect_equal(max_chroma_calc, max_chroma, tolerance = 0.1)
  }

  expect_match_chroma(hex, pth_to_cielab)
  expect_match_chroma(hex, pth_to_cieluv)
  expect_match_chroma(hex, pth_to_cam02ucs)
  expect_match_chroma(hex, pth_to_cam16ucs)
  expect_match_chroma(hex, pth_to_jzazbz100)

})

test_that("pth_clip_chroma() works", {

  expect_error(pth_clip_chroma(3), "No method")

  expect_identical(
    pth_clip_chroma("#FFFFFF"),
    pth_to_hex("#FFFFFF")
  )

  luv_test <-
    pth_to_cieluv(c("#000000", "#663399", "#ff1122", "#ff4500", "#ffffff"))

  expect_equal(
    pth_clip_chroma(luv_test),
    luv_test,
    tolerance = 0.1
  )

  # exceed chroma

  # at gamut outer-surface
  luv_test <-
    pth_to_cieluv(c("#000000", "#ff1122", "#ff4500", "#ffffff"))

  polar <- pth_to_polar(luv_test)
  polar[, 2] <- polar[, 2] + 10 # go outside gamut

  luv_new <- pth_new_cieluv(pth_to_cartesian(polar))

  expect_equal(
    pth_clip_chroma(luv_new),
    luv_test,
    tolerance = 0.1,
    ignore_attr = TRUE
  )

})
