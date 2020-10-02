
The purpose of this document is to generate package-data that describe a
color path:

  - a data-frame with HCL values
  - a matrix with LUV values

<!-- end list -->

``` r
library("tibble")
library("farver")
library("usethis")
```

First, let’s create a data frame:

``` r
df_hcl_blues <- 
  tribble(
     ~h,  ~c,  ~l,
    250,   0,  25,
    250,  75,  55,
    250,   0,  85
  ) %>%
  print()
```

    ## # A tibble: 3 x 3
    ##       h     c     l
    ##   <dbl> <dbl> <dbl>
    ## 1   250     0    25
    ## 2   250    75    55
    ## 3   250     0    85

Let’s put a floor on chroma.

``` r
df_hcl_blues[["c"]] <- pmax(df_hcl_blues[["c"]], 1.e-2)
```

Next, we create a matrix:

``` r
mat_hcl_blues <- 
  df_hcl_blues %>%
  as.matrix() %>% 
  print()
```

    ##        h     c  l
    ## [1,] 250  0.01 25
    ## [2,] 250 75.00 55
    ## [3,] 250  0.01 85

And convert it to LUV format:

``` r
mat_luv_blues <- 
  mat_hcl_blues %>%
  farver::convert_colour(from = "hcl", to = "luv") %>%
  print()
```

    ##       l             u             v
    ## [1,] 25  -0.003423786  -0.009393968
    ## [2,] 55 -25.651507450 -70.476945208
    ## [3,] 85  -0.003432393  -0.009386866

We want to write out the HCL data frame and the LUV matrix:

``` r
use_data(df_hcl_blues, mat_luv_blues, overwrite = TRUE)
```

    ## ✓ Setting active project to '/Users/sesa19001/Documents/repos/public/colorpath'

    ## ✓ Saving 'df_hcl_blues', 'mat_luv_blues' to 'data/df_hcl_blues.rda', 'data/mat_luv_blues.rda'

    ## ● Document your data (see 'https://r-pkgs.org/data.html')
