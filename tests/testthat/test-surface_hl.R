sfc_blues_single <- surface_hl(250)
sfc_blues_multi <- surface_hl(c(240, 260))
df_cl <- data.frame(l = c(20, 50, 80), c = c(0, 150, 0))

test_that("surface_hl works()", {

  expect_error(surface_hl("foo"), "not a numeric")
  expect_error(surface_hl(c(1, 2, 3)), "less than")
  expect_error(surface_hl(numeric(0)), "greater than")

  expect_error(surface_hl(250, input_lum = 20), "length")
  expect_error(surface_hl(250, input_lum = "foo"), "numeric")
  expect_error(surface_hl(250, input_lum = c(20, 20)), "distinct")

  expect_type(sfc_blues_single, "closure")
  expect_s3_class(sfc_blues_single, "cpath_surface_hl")

  expect_identical(
    sfc_blues_single(c(0, 50, 100)),
    c(250, 250, 250)
  )

  expect_identical(
    sfc_blues_multi(c(0, 50, 100)),
    c(240, 250, 260)
  )

})

test_that("df_hcl() works", {

  expect_error(df_hcl("foo", sfc_blues_multi), "not a data frame")
  expect_error(
    df_hcl(mtcars, sfc_blues_multi),
    "does not have all of these name\\(s\\)"
  )
  expect_error(df_hcl(df_cl, "foo"), "not a function")

  df_hcl_blues_multi <- df_hcl(df_cl, sfc_blues_multi)

  expect_equal(
    df_hcl_blues_multi,
    tibble::tibble(
      h = c(244, 250, 256),
      c = c(0, 150, 0),
      l = c(20, 50, 80)
    )
  )

})

test_that("mat_chroma() works", {

  hue <- 250
  luminance <- 50
  chroma_max <- 86.65
  step <- 2

  n <- floor(chroma_max / 2)

  mat_cma <- mat_chroma(hue, luminance, chroma_max, step)

  expect_type(mat_cma, "double")
  expect_true(is.matrix(mat_cma))
  expect_identical(dimnames(mat_cma), list(NULL, c("h", "c", "l")))

  expect_identical(mat_cma[, "h"], rep(hue, n))
  expect_identical(mat_cma[, "c"], seq(from = step / 2, by = step, length.out = n))
  expect_identical(mat_cma[, "l"], rep(luminance, n))

})

test_that("mat_surface_hl() works", {

  expect_error(mat_surface_hl("foo"), "does not inherit")

  mat_sfc <- mat_surface_hl(sfc_blues_multi)

  expect_type(mat_sfc, "double")
  expect_true(is.matrix(mat_sfc))
  expect_identical(dimnames(mat_sfc), list(NULL, c("h", "c", "l")))

})

test_that("data_surface_hl() works", {

  expect_error(data_surface_hl("foo"), "does not inherit")

  df_sfc <- data_surface_hl(sfc_blues_multi)

  expect_s3_class(df_sfc, "tbl")
  expect_named(df_sfc, c("h", "c", "l", "hex"))

})


test_that("plot_surface_hl() works", {

  expect_error(plot_surface_hl("foo"), "does not inherit")

  plot_sfc <- plot_surface_hl(sfc_blues_multi)

  expect_s3_class(plot_sfc, "gg")

})

