cvd <- pth_cvd_grid_severity()

# categorical
hex_cat <- c("#FFFFFF", "#777777", "#000000")
mat_cat <- pth_to_cieluv(hex_cat)

list_mat_cat <- list_mat(mat_cat)

color_cvd_cat <- color_cvd_cat(list_mat_cat, cvd)

data_fill_cat <- data_fill(color_cvd_cat)

# quantitative
palette_qnt <- pth_new_palette_hex(hex_cat)

color_qnt <- palette_qnt(seq(0, 1, length.out = 12))
list_mat_qnt <- list_mat(color_qnt)

color_cvd_qnt <- color_cvd_qnt(list_mat_qnt, cvd)
data_fill_qnt <- data_fill(color_cvd_qnt)

# calculate distance (Euclid)
data_euclid_cat <- data_fill_cat
data_euclid_cat[["distance"]] <-
  purrr::pmap_dbl(
    data_fill_cat[, c("color_a", "color_b")],
    pth_distance_euclid
  )

data_euclid_qnt <- data_fill_qnt
data_euclid_qnt[["distance"]] <-
  purrr::pmap_dbl(
    data_fill_qnt[, c("color_a", "color_b")],
    pth_distance_euclid
  )

# calculate distance (metric)
data_metric_cat <- data_fill_cat
data_metric_cat[["distance"]] <-
  purrr::pmap_dbl(
    data_fill_cat[, c("color_a", "color_b")],
    pth_distance_metric
  )

data_metric_qnt <- data_fill_qnt
data_metric_qnt[["distance"]] <-
  purrr::pmap_dbl(
    data_fill_qnt[, c("color_a", "color_b")],
    pth_distance_metric
  )

result_euclid_cat <- data_select(data_euclid_cat)
result_euclid_qnt <- data_select(data_euclid_qnt)

result_metric_cat <- data_select(data_metric_cat)
result_metric_qnt <- data_select(data_metric_qnt)

test_that("data_list_mat() works", {
  expect_identical(list_mat_cat, list(mat_cat[1, ], mat_cat[2, ], mat_cat[3, ]))
})

test_that("color_cvd_cat() works", {
  expect_identical(
    nrow(color_cvd_cat),
    length(list_mat_cat) * length(list_mat_cat) * nrow(cvd)
  )
})

test_that("data_fill() works", {
  expect_named(
    data_fill_cat,
    c("color_original_a", "color_original_b", "condition", "severity",
      "hex_original_a", "hex_original_b", "color_a", "color_b",
      "hex_a", "hex_b")
  )
  expect_identical(nrow(color_cvd_cat), nrow(data_fill_cat))
})

test_that("data_select() works", {
  expect_named(
    result_euclid_cat,
    c("hex_original_a", "hex_original_b", "condition", "severity",
      "hex_a", "hex_b", "distance")
  )
  expect_identical(nrow(result_euclid_cat), nrow(data_fill_cat))
})

test_that("pth_data_cat_euclid.character() works", {
  expect_identical(pth_data_cat_euclid(hex_cat), result_euclid_cat)
})

test_that("pth_data_cat_euclid.pth_mat() works", {
  expect_identical(pth_data_cat_euclid(mat_cat), result_euclid_cat)
})
test_that("pth_data_cat_metric.character() works", {
  expect_identical(pth_data_cat_metric(hex_cat), result_metric_cat)
})

test_that("pth_data_cat_metric.pth_mat() works", {
  expect_identical(pth_data_cat_metric(mat_cat), result_metric_cat)
})

test_that("pth_data_qnt_euclid.pth_palette() works", {
  expect_identical(pth_data_qnt_euclid(palette_qnt), result_euclid_qnt)
})

test_that("pth_data_qnt_metric.pth_palette() works", {
  expect_identical(pth_data_qnt_metric(palette_qnt), result_metric_qnt)
})

test_that("color_cvd_qnt() works", {
  expect_identical(
    nrow(color_cvd_qnt),
    (length(list_mat_qnt) - 1L) * nrow(cvd)
  )
})
