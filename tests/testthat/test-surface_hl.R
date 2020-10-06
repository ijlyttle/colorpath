test_that("surface_hl works()", {

  expect_error(surface_hl("foo"), "not a numeric")
  expect_error(surface_hl(c(1, 2, 3)), "less than")
  expect_error(surface_hl(numeric(0)), "greater than")

  expect_error(surface_hl(250, input_lum = 20), "length")
  expect_error(surface_hl(250, input_lum = "foo"), "numeric")
  expect_error(surface_hl(250, input_lum = c(20, 20)), "distinct")

  f_single <- surface_hl(250)
  f_multi <- surface_hl(c(240, 260))

  expect_type(f_single, "closure")

  expect_identical(
    f_single(c(0, 50, 100)),
    c(250, 250, 250)
  )

  expect_identical(
    f_multi(c(0, 50, 100)),
    c(240, 250, 260)
  )

})

test_that("df_hcl() works", {

  sfc_blues_multi <- surface_hl(c(240, 260))
  df_cl <- data.frame(l = c(20, 50, 80), c = c(0, 150, 0))

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
