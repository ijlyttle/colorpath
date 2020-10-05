
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
    250,   0,  30,
    250, 150,  60,
    250,   0,  90
  ) %>%
  print()
```

    ## # A tibble: 3 x 3
    ##       h     c     l
    ##   <dbl> <dbl> <dbl>
    ## 1   250     0    30
    ## 2   250   150    60
    ## 3   250     0    90

Let’s put a floor on chroma.

``` r
df_chroma <- df_hcl_blues
df_chroma[["c"]] <- pmax(df_hcl_blues[["c"]], 1.e-2)
```

Next, we create a matrix:

``` r
mat_hcl_blues <- 
  df_chroma %>%
  as.matrix() %>% 
  print()
```

    ##        h      c  l
    ## [1,] 250   0.01 30
    ## [2,] 250 150.00 60
    ## [3,] 250   0.01 90

And convert it to LUV format:

``` r
mat_luv_blues <- 
  mat_hcl_blues %>%
  farver::convert_colour(from = "hcl", to = "luv") %>%
  print()
```

    ##       l             u             v
    ## [1,] 30  -0.003424503 -9.393376e-03
    ## [2,] 60 -51.303007837 -1.409539e+02
    ## [3,] 90  -0.003433110 -9.386274e-03

We want to write out the HCL data frame and the LUV matrix:

``` r
use_data(df_hcl_blues, mat_luv_blues, overwrite = TRUE)
```

    ## ✓ Setting active project to '/Users/sesa19001/Documents/repos/public/colorpath'

    ## ✓ Saving 'df_hcl_blues', 'mat_luv_blues' to 'data/df_hcl_blues.rda', 'data/mat_luv_blues.rda'

    ## ● Document your data (see 'https://r-pkgs.org/data.html')
