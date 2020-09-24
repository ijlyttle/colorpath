
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
df_hcl <- 
  tribble(
     ~h,  ~c,  ~l,
    130,   0,  20,
    130,  73,  50,
    130,   0,  80
  ) %>%
  print()
```

    ## # A tibble: 3 x 3
    ##       h     c     l
    ##   <dbl> <dbl> <dbl>
    ## 1   130     0    20
    ## 2   130    73    50
    ## 3   130     0    80

Next, we create a matrix:

``` r
mat_hcl <- 
  df_hcl %>%
  as.matrix() %>% 
  print()
```

    ##        h  c  l
    ## [1,] 130  0 20
    ## [2,] 130 73 50
    ## [3,] 130  0 80

And convert it to LUV format:

``` r
mat_luv <- 
  mat_hcl %>%
  farver::convert_colour(from = "hcl", to = "luv") %>%
  round(digits = 4) %>%
  print()
```

    ##       l        u       v
    ## [1,] 20   0.0000  0.0000
    ## [2,] 50 -46.9235 55.9212
    ## [3,] 80   0.0000  0.0000

We want to write out the HCL data frame and the LUV matrix:

``` r
use_data(df_hcl, mat_luv, overwrite = TRUE)
```

    ## ✓ Setting active project to '/Users/sesa19001/Documents/repos/public/colorpath'

    ## ✓ Adding 'R' to Depends field in DESCRIPTION

    ## ✓ Creating 'data/'

    ## ✓ Saving 'df_hcl', 'mat_luv' to 'data/df_hcl.rda', 'data/mat_luv.rda'

    ## ● Document your data (see 'https://r-pkgs.org/data.html')
