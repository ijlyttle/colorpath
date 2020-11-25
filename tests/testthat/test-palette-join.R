hex_blue <- c("#e2e2e2", "#9cbaee", "#3c79c0")  %>% pth_to_hex()
hex_orange <- c("#e2e2e2", "#e0af85", "#a66a00")  %>% pth_to_hex()

pal_hex_blue <- pth_new_palette_hex(hex_blue)
pal_hex_orange <- pth_new_palette_hex(hex_orange)

sfc_blue <- pth_new_hue_surface(250)
sfc_orange <- pth_new_hue_surface(45)

traj <-
  pth_new_chroma_trajectory(
    chroma = c(0, 75, 75, 50),
    lum = c(90, 70, 50, 35)
  )

pal_path_blue <- pth_new_palette_path(traj, sfc_blue)
pal_path_orange <- pth_new_palette_path(traj, sfc_orange)

pal_path_blue_lab <-
  pth_new_palette_path(traj, sfc_blue, constructor = pth_new_cielab)

test_that("internal palette_join() works", {

  # error if palette-functions not same class
  expect_error(
    palette_join(pal_hex_blue, pal_path_orange),
    "class\\(palette_low\\)"
  )

  # error if palette-functions use non-identical color spaces
  expect_error(
    palette_join(pal_path_blue, pal_path_blue_lab),
    "attributes\\(test_low\\)"
  )

  pal_join_hex <- palette_join(pal_hex_blue, pal_hex_orange)

  # expect only a function at this point
  expect_type(pal_join_hex, "closure")

  # expect output to be cieluv
  expect_identical(
    class(pal_join_hex(0)),
    c("pth_cieluv", "pth_mat")
  )

  # recover the original hex codes
  expect_identical(
    pal_join_hex(c(seq(0, 1, by = 0.25))) %>% pth_to_hex(),
    pth_to_hex(c(rev(hex_blue), tail(hex_orange, -1)))
  )

  pal_join_path <- palette_join(pal_path_blue, pal_path_orange)

  # expect only a function at this point
  expect_type(pal_join_path, "closure")

  # expect output to be cieluv
  expect_identical(
    class(pal_join_path(0)),
    c("pth_cieluv", "pth_mat")
  )

  # this is especially stinky
  expect_identical(
    pal_join_path(c(seq(0, 1, by = 0.25))) %>% pth_to_hex(),
    c(
      pal_path_blue(c(1, 0.5, 0)) %>% pth_to_hex(),
      pal_path_orange(c(0.5, 1)) %>% pth_to_hex()
    ) %>% pth_to_hex()
  )

})

test_that("pth_palette_join() works", {

  expect_error(
    pth_palette_join("foo", pal_hex_orange),
    "character"
  )

  # hex works

  pal_join_hex <- pth_palette_join(pal_hex_blue, pal_hex_orange)

  # expect only a function at this point
  expect_type(pal_join_hex, "closure")

  # expect output to be cieluv
  expect_identical(
    class(pal_join_hex(0)),
    c("pth_cieluv", "pth_mat")
  )

  # recover the original hex codes
  expect_identical(
    pal_join_hex(c(seq(0, 1, by = 0.25))) %>% pth_to_hex(),
    pth_to_hex(c(rev(hex_blue), tail(hex_orange, -1)))
  )

  # attributes
  expect_s3_class(pal_join_hex, c("pth_palette_hex", "pth_palette"))
  expect_identical(
    attr(pal_join_hex, "nodes"),
    list(hex_blue, hex_orange)
  )

  pal_join_path <- pth_palette_join(pal_path_blue, pal_path_orange)

  # expect only a function at this point
  expect_type(pal_join_path, "closure")

  # expect output to be cieluv
  expect_identical(
    class(pal_join_path(0)),
    c("pth_cieluv", "pth_mat")
  )

  # this is especially stinky
  expect_identical(
    pal_join_path(c(seq(0, 1, by = 0.25))) %>% pth_to_hex(),
    c(
      pal_path_blue(c(1, 0.5, 0)) %>% pth_to_hex(),
      pal_path_orange(c(0.5, 1)) %>% pth_to_hex()
    ) %>% pth_to_hex()
  )

  # attributes
  expect_s3_class(pal_join_path, c("pth_palette_path", "pth_palette"))
  expect_identical(
    attr(pal_join_path, "control_points"),
    c(
      attr(pal_path_blue, "control_points"),
      attr(pal_path_orange, "control_points")
    )
  )
  expect_identical(
    attr(pal_join_path, "surface"),
    c(
      attr(pal_path_blue, "surface"),
      attr(pal_path_orange, "surface")
    )
  )
})

