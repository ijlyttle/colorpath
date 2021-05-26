test_that("get_angles() works", {

  # validate angles
  expect_snapshot_error(get_angles(angles = NULL, "short"))
  expect_snapshot_error(get_angles(angles = numeric(0), "short"))
  expect_snapshot_error(get_angles(angles = c(0, 0, 0), "short"))

  # validate route
  expect_snapshot_error(get_angles(angles = 0, route = 3))
  expect_snapshot_error(get_angles(angles = 0, route = "not_long"))

  # one angle
  expect_identical(get_angles(angles = 0, "short"), c(0, 0))
  expect_identical(get_angles(angles = 0, "long"), c(0, 0))

  # in degrees
  expect_identical(get_angles(angles = 365, "short"), c(5, 5))

  # two angles
  expect_identical(get_angles(angles = c(0, 0), "short"), c(0, 0))
  expect_identical(get_angles(angles = c(0, 0), "long"), c(0, 0))

  # we differentiate between long and short
  expect_identical(get_angles(angles = c(2, 20), "short"), c(2, 20))
  expect_identical(get_angles(angles = c(2, 20), "long"), c(362, 20))

  expect_identical(get_angles(angles = c(20, 2), "short"), c(20, 2))
  expect_identical(get_angles(angles = c(20, 2), "long"), c(-340, 2))

})

test_that("path_includes_branch_cut() works", {

  expect_true(path_includes_branch_cut(c(359, 1), "short"))
  expect_false(path_includes_branch_cut(c(359, 1), "long"))

  expect_true(path_includes_branch_cut(c(2, 190), "short"))
  expect_false(path_includes_branch_cut(c(2, 190), "long"))

  expect_true(path_includes_branch_cut(c(2, 170), "long"))
  expect_false(path_includes_branch_cut(c(2, 170), "short"))

})
