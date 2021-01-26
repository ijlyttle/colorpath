hex <- "#663399"

test_that("pth_cvd_grid() works", {

  # input
  expect_error(pth_cvd_grid(condition = "foo"), "should be one of")
  expect_error(pth_cvd_grid(condition = "none", severity = -1), "severity >= 0")
  expect_error(pth_cvd_grid(condition = "none", severity = 2), "severity <= 1")

  # output
  condition <- c("none", "deutan")
  severity <- c(0, 0.5, 1)

  cvd_grid <- pth_cvd_grid(condition = condition, severity = severity)

  expect_s3_class(cvd_grid, "tbl_df")
  expect_named(cvd_grid, c("condition", "severity"))
  expect_identical(nrow(cvd_grid), length(condition) * length(severity))
  expect_identical(
    unique(cvd_grid$condition),
    factor(condition, levels = condition)
  )
  expect_identical(unique(cvd_grid$severity), severity)

  # pth_cvd_grid_full, pth_cvd_grid_none are syntactic sugar
  expect_s3_class(pth_cvd_grid_full(), "tbl_df")
  expect_s3_class(pth_cvd_grid_none(), "tbl_df")

})

test_that("rgb_cvd", {

  test_rgb <- to_rgb(c("#00FF00", "#663399", "#FF00FF"))

  # input
  expect_error(
    rgb_cvd(test_rgb, condition = "foo", severity = 1),
    "should be one of"
  )

  expect_error(
    rgb_cvd(test_rgb, condition = "none", severity = -1),
    "severity not greater"
  )

  expect_error(
    rgb_cvd(test_rgb, condition = "none", severity = 2),
    "severity not less"
  )

  expect_error(
    rgb_cvd(test_rgb, condition = "none", severity = c(1, 1)),
    "length one"
  )

  # output
  expect_cvd <- function(mat_rgb, condition, severity) {

    result <- rgb_cvd(mat_rgb, condition, severity)

    if (identical(condition, "none")) {
      expect_identical(result, pth_new_srgb255(mat_rgb))
      return(NULL)
    }

    # need to round to compare to colorspace expectation
    result <- round(result, 0)

    list_transform <- list(
      deutan = colorspace::deutan,
      protan = colorspace::protan,
      tritan = colorspace::tritan
    )

    trans <- list_transform[[condition]]

    # use colorspace function to make cvd transformation
    expectation <- trans(t(mat_rgb), severity) %>% t()
    dimnames(expectation) <- list(NULL, c("r", "g", "b"))
    expectation <- pth_new_srgb255(expectation)

    # check
    expect_identical(result, expectation)
  }

  expect_cvd(test_rgb, "none", 1)
  expect_cvd(test_rgb, "deutan", 1)
  expect_cvd(test_rgb, "deutan", 0.5)
  expect_cvd(test_rgb, "deutan", 0.55)
  expect_cvd(test_rgb, "protan", 0.55)
  expect_cvd(test_rgb, "tritan", 0.55)

})

test_that("mat_cvd() works", {

  # will test only the identity here because other cvd transformations
  #  already tested by rgb_cvd()

  mat_luv <- pth_to_cieluv(hex)

  expect_equal(
    mat_cvd(mat_luv, "none", 1),
    mat_luv
  )

})

test_that("pth_data_cvd() works", {

  # inputs
  hex <- c("#112233", "#663399")
  condition <- c("none", "deutan", "protan", "tritan")
  severity <- c(0, 0.5, 1)
  grid <- pth_cvd_grid(condition, severity)

  cvd <- pth_data_cvd(hex, grid)

  # nature of output
  expect_s3_class(cvd, "tbl_df")
  expect_named(
    cvd,
    c("condition", "severity", "index_color", "luminance", "chroma", "hue", "hex")
  )
  expect_identical(
    nrow(cvd),
    length(hex) * length(condition) * length(severity)
  )
  expect_s3_class(cvd$condition, "factor")

  # correctness
  rgb_cvd <- function(condition, severity, index_color, hex_old, ...) {

    xform <- list(
      none = function(col, severity) { col },
      deutan = colorspace::deutan,
      protan = colorspace::protan,
      tritan = colorspace::tritan
    )

    hex_new <- xform[[condition]](hex_old[index_color], severity)

    hex_new
  }

  hex_ref <- purrr::pmap(cvd, rgb_cvd, hex_old = hex) %>% unlist()
  hex_calc <- cvd$hex

  # we get rounding errors in all the transformations
  # - we want to make sure that the largest difference is 1
  expect_true(
    max(abs(to_rgb(hex_ref) - to_rgb(hex_calc))) <= 1
  )

})
