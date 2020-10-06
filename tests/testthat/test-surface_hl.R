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

test_that("points_hcl_surface() works", {

  hcl_multi <- points_hcl_surface(sfc_blues_multi)

  expect_s4_class(hcl_multi, "polarLUV")

  expect_equal(hcl_multi@coords[, "H"], c(248, 252))
  expect_equal(hcl_multi@coords[, "C"], c( 30,  30))
  expect_equal(hcl_multi@coords[, "L"], c( 40,  60))

})
