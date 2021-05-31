get_angles <- function(angles, route) {

  # validate
  assertthat::assert_that(
    is.numeric(angles),
    length(angles) == 1 || length(angles) == 2,
    assertthat::is.string(route),
    route == "short" || route == "long"
  )

  # angles are in degrees
  angles <- angles %% 360

  # if one angle sent create a second
  if (identical(length(angles), 1L)) {
    angles[2] <- angles[1]
  }

  # if angles are identical, return
  if (identical(angles[2], angles[1])) {
    return(angles)
  }

  if (path_includes_branch_cut(angles, route)) {
    if (angles[2] > angles[1]) {
      angles[1] <- angles[1] + 360
    } else {
      angles[1] <- angles[1] - 360
    }
  }

  angles
}

path_includes_branch_cut <- function(angles, route) {
  (route == "short") == (abs(angles[2] - angles[1]) > 180)
}
