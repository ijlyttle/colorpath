test_that("pth_new_hue_surface() works", {

  sfc_blues_single <- pth_new_hue_surface(250)
  sfc_blues_multi <- pth_new_hue_surface(c(240, 260))

  expect_error(pth_new_hue_surface("foo"), "not a numeric")
  expect_error(pth_new_hue_surface(c(1, 2, 3)), "less than")
  expect_error(pth_new_hue_surface(numeric(0)), "greater than")

  expect_error(pth_new_hue_surface(250, lum = 20), "length")
  expect_error(pth_new_hue_surface(250, lum = "foo"), "numeric")
  expect_error(pth_new_hue_surface(250, lum = c(20, 20)), "distinct")

  expect_type(sfc_blues_single, "closure")
  expect_s3_class(sfc_blues_single, "pth_hue_surface")

  expect_identical(
    sfc_blues_single(c(0, 50, 100)),
    c(250, 250, 250)
  )

  expect_identical(
    sfc_blues_multi(c(0, 50, 100)),
    c(240, 250, 260)
  )

})

test_that("pth_new_chroma_trajectory() works", {

  chroma <- c(0, 100, 0)
  lum <- c(20, 50, 80)

  # check validation
  expect_error(pth_new_chroma_trajectory("foo", lum), "chroma.*numeric")
  expect_error(pth_new_chroma_trajectory(chroma, "foo"), "lum.*numeric")
  expect_error(pth_new_chroma_trajectory(chroma, 0), "length\\(chroma\\)")
  expect_error(
    pth_new_chroma_trajectory(numeric(0), numeric(0)),
    "length\\(chroma\\) not greater than"
  )
  expect_error(
    pth_new_chroma_trajectory(-chroma, lum),
    "chroma >= 0"
  )

  # check results
  traj <- pth_new_chroma_trajectory(chroma, lum)

  expect_type(traj, "closure")
  expect_s3_class(traj, "pth_chroma_trajectory")

  expect_identical(
    attr(traj, "control_points"),
    structure(
      matrix(c(lum, chroma), ncol = 2, byrow = FALSE),
      dimnames = list(NULL, c("lum", "chroma"))
    )
  )

  expect_identical(
    traj(c(0, 0.5, 1)),
    structure(
      matrix(c(c(20, 50, 80), c(0, 50, 0)), ncol = 2, byrow = FALSE),
      dimnames = list(NULL, c("lum", "chroma"))
    )
  )


})
