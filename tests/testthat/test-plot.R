test_that("df_hcl works", {

  mat_hcl_blues <- farver::convert_colour(mat_luv_blues, from = "luv", to = "hcl")

  df_hcl_blues <- df_hcl(mat_hcl_blues, type = "control point", use_hex = FALSE)

  expect_s3_class(df_hcl_blues, "tbl_df")
  expect_named(
    df_hcl_blues,
    c("type", "hex", "hue", "chroma", "luminance", "label")
  )

  expect_identical(
    df_hcl_blues[["hex"]],
    rep("#777777", nrow(mat_hcl_blues))
  )

  expect_identical(
    df_hcl_blues[["type"]],
    factor_type(rep("control point", nrow(mat_hcl_blues)))
  )

  expect_identical(
    df_hcl_blues[["label"]],
    rep("hue = 250", nrow(mat_hcl_blues))
  )

})
