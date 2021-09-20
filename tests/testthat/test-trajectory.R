test_that("pth_new_trajectory() works", {

  lum <- c(20, 50, 80)
  chroma <- c(0, 100, 0)

  # check validation
  expect_error(pth_new_trajectory(lum, "foo"), "chroma.*numeric")
  expect_error(pth_new_trajectory("foo", chroma), "lum.*numeric")
  expect_error(pth_new_trajectory(0, chroma), "length\\(chroma\\)")
  expect_error(
    pth_new_trajectory(numeric(0), numeric(0)),
    "length\\(chroma\\) not greater than"
  )
  expect_error(
    pth_new_trajectory(lum, -chroma),
    "chroma >= 0"
  )

  # check results
  traj <- pth_new_trajectory(lum, chroma)

  expect_type(traj, "closure")
  expect_s3_class(traj, "pth_trajectory")

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
