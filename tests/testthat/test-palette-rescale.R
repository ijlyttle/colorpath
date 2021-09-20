# not perceptually uniform
pal_grey <- pth_new_palette_hex(c("#000000", "#666666", "#ffffff"))

test_that("pth_palette_rescale_reverse() works", {

  viridis_11 <- hcl.colors(11, "viridis")
  x_11 <- seq(0, 1, length.out = 11)

  pal_viridis <- pth_new_palette_hex(viridis_11)
  pal_viridis_reverse <- pth_palette_rescale_reverse(pal_viridis)

  expect_error(
    pth_palette_rescale_reverse("foo"),
    "pth_palette"
  )

  expect_type(pal_viridis_reverse, "closure")
  expect_s3_class(pal_viridis_reverse, c("pth_palette_hex", "pth_palette"))

  expect_identical(
    pal_viridis_reverse(x_11),
    pal_viridis(1 - x_11)
  )

})

test_that("pth_palette_rescale_domain() works", {

  viridis_11 <- hcl.colors(11, "viridis")
  x_11 <- seq(0, 1, length.out = 11)

  pal_viridis <- pth_new_palette_hex(viridis_11)
  pal_viridis_domain <-
    pth_palette_rescale_domain(pal_viridis, domain = c(0.25, 0.75))

  expect_error(
    pth_palette_rescale_domain("foo"),
    "pth_palette"
  )

  expect_type(pal_viridis_domain, "closure")
  expect_s3_class(pal_viridis_domain, c("pth_palette_hex", "pth_palette"))

  expect_identical(
    pal_viridis_domain(x_11),
    pal_viridis(seq(0.25, 0.75, length.out = 11))
  )

})

test_that("pth_palette_rescale_euclid() works", {

  expect_error(
    pth_palette_rescale_euclid(
      palette = "foo",
      tolerance = 1.e-4,
      non_luminance_weight = 1,
      transformer = identity
    ),
    "pth_palette"
  )

  expect_error(
    pth_palette_rescale_euclid(
      palette = pal_grey,
      tolerance = "foo",
      non_luminance_weight = 1,
      transformer = identity
    ),
    "tolerance"
  )

  expect_error(
    pth_palette_rescale_euclid(
      palette = pal_grey,
      tolerance = 1.e-4,
      non_luminance_weight = "foo",
      transformer = identity
    ),
    "non_luminance_weight"
  )

  expect_error(
    pth_palette_rescale_euclid(
      palette = pal_grey,
      tolerance = 1.e-4,
      non_luminance_weight = 1,
      transformer = "foo"
    ),
    "transformer"
  )

  pal_grey_rescale <-
    pth_palette_rescale_euclid(
      palette = pal_grey,
      tolerance = 1.e-4,
      non_luminance_weight = 1,
      transformer = identity
    )

  expect_type(pal_grey_rescale, "closure")
  expect_s3_class(pal_grey_rescale, "pth_palette")

  # TODO: why is this #757575?
  expect_identical(
    pth_to_hex(pal_grey_rescale(c(0, 0.5, 1))),
    pth_to_hex(c("#000000", "#757575", "#ffffff"))
  )
})

test_that("pth_palette_rescale_metric() works", {

  expect_error(
    pth_palette_rescale_metric(
      palette = "foo",
      tolerance = 1.e-4,
      method = "cie2000"
    ),
    "pth_palette"
  )

  expect_error(
    pth_palette_rescale_metric(
      palette = pal_grey,
      tolerance = "foo",
      method = "cie2000"
    ),
    "tolerance"
  )

  expect_error(
    pth_palette_rescale_metric(
      palette = pal_grey,
      tolerance = 1.e-4,
      method = "foo"
    ),
    "arg"
  )

  pal_grey_rescale <-
    pth_palette_rescale_metric(
      palette = pal_grey,
      tolerance = 1.e-4,
      method = "cie2000"
    )

  expect_type(pal_grey_rescale, "closure")
  expect_s3_class(pal_grey_rescale, "pth_palette")

  expect_identical(
    pth_to_hex(pal_grey_rescale(c(0, 0.5, 1))),
    pth_to_hex(c("#000000", "#777777", "#ffffff"))
  )
})
