# colorpath 0.0.0 (development version)

* Refactored (#1, continued):

  - renamed S3 class `cpath_palette_luv` to `cpath_pal_luv`.
  - added function `as_pal_disc()` to coerce to a discrete-palette function.
  - renamed `palette_hex()` to `as_pal_hex()`.
  - renamed function `luv()` to `as_mat_luv()`.
  - renamed argument `luv` to `mat_luv`.

* Added function to convert LUV palette-function to hex-code palette-function, `palette_hex()`. (#14)

* Added function to rescale a palette function, `rescale_palette()`. (#10)

* Added rescaler function `rescaler_linear_luminance()`. (#11)

* Added datasets `df_hcl_blues` and `mat_luv_blues`, which describe the same example colorpath. (#8)

* Added rescaler functions: `rescaler_linear_input()` and `rescaler_bezier()`. (#6, #11)

* Added function `palette_bezier()` to create a palette function using a Bézier spline. (#4)

* Added function `luv()` to convert HCL data frame to LUV matrix. (#2)

* Added a `NEWS.md` file to track changes to the package.
