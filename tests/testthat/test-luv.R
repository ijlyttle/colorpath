library("tibble")

test_that("luv() works", {

  df_hcl <- tribble(
    ~h,  ~c,  ~l,
     0,  50,  45,
    30,  70,  50
  )

  mat_luv <- matrix(
    c(
      45,         50, # l
      50,   60.62178, # u
       0,         35  # v
    ),
    ncol = 3L,
    byrow = FALSE,
    dimnames = list(NULL, c("l", "u", "v"))
  )

  expect_equal(luv(df_hcl), mat_luv, tolerance = 1.e-4)

})
