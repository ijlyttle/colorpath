# colorpath 0.0.1 (development version)

* Reworked the philosophy of this package so that we can consider a class of color spaces that I call Lab-100-class. 
  These color spaces:
  
  - are described using dimensions luminance, blue-yellow, green-red.
  - the value of black is `c(0, 0, 0)`; the value of white is approximately `c(100, 0, 0)`.
  - aspire to be perceptually uniform: the difference between colors scales with the Euclidean distance between colors.
  - have polar-coordinate analogues using hue and chroma.
  
  Such color spaces impleneted here:
  
  - CIELAB
  - CIELUV
  - CAM02-UCS
  - CAM16-UCS
  - Jzazbz-100

  New color-space functions (#44):
  
  - `pth_to_cartesian()`, `pth_to_polar()`: to convert back-and-forth among coordinate systems.
    Note that `pth_to_cartesian()` has an argument `chroma_min`, used to preserve the hue where the chroma is zero.
    This means that values where chroma is at or near zero are imputed with an imperceptably small chroma, allowing the
    hue to be recovered when `pth_to_polar()` is used.
  
  - `pth_to_hex()`, `pth_new_hex()`: Convert to hex codes, or designate a character vector as hex codes.
  
  - Convert to matrix using a color space, or designate that a matrix uses a color space:
  
    - `pth_to_cielab()`, `pth_new_cielab()`: CIELAB
    - `pth_to_cieluv()`, `pth_new_cieluv()`: CIELUV
    - `pth_to_cam02ucs()`, `pth_new_cam02ucs()`: CAM02-UCS
    - `pth_to_cam16ucs()`, `pth_new_cam16ucs()`: CAM16-UCS
    - `pth_to_jzazbz100()`, `pth_new_jzazbz100()`: Jzazbz, scaled to 100

  - `whitepoints_cie1931()`: Some of the color spaces need a reference white-point; this helper function provides the values. 
    The default, `"D65"`, seems to be used widely. 
  
  The functions `pth_to_hex()`, `pth_to_cieluv()`, etc., form a "boat" of sorts. 
  You can pipe values from one such function to another; you will always be describing the same colors. 
  i.e. you will not "fall out of the boat". 
  However, the functions `pth_to_cartesian()` and `pth_to_polar()` kick you "out of the boat".
  To get back "into the boat", you will need to use one of the `pth_new_()` functions.
  
  New gamut functions (#46):
  
  - `pth_in_gamut()`: Indicates if a color is inside (not outside) the RGB gamut.
  - `pth_max_chroma()`: Returns the maximum chroma for each color, expressed using its color space.
  - `pth_clip_chroma()`: Returns the colors, capping the chromas for out-of-gamut colors to their maximum chromas.
  
  New functions to suport distance calculations (#48):
  
  - `pth_distance_euclid()`: Returns Euclidean distances between colors, given a color space. 
  - `pth_distance_metric()`: Returns metric-based distances between colors.
  - `pth_n_color()`: Returns the number of colors in a vector or matrix.
  - `pth_mat_replace_data()`: Returns a `pth_mat` with new data.
  
  New functions to support palettes. A palette (function) returns a color for each given value between 0 and 1 (#50):
  
  - `pth_new_hue_surface()`: Returns a function (with S3 class `pth_hue_surface`) that, for each given luminance, returns a hue.
  - `pth_new_chroma_trajectory()`: Returns a function (with S3 class `pth_chroma_trajectory`) that, for each given x (between 0 and 1), returns a set of luminance and chroma; uses a Bézier curve under the hood.
  
  - `pth_new_palette()`: Returns a palette function (with S3 class `pth_palette`), given a hue-surface, chroma-trajectory, and a color space.
  - `pth_new_palette_hex()`: Returns a palette function, given a vector of hex-codes and an optional tranformer-function.
  
# colorpath 0.0.0 (development version)

* Added function `get_distance()` to calculate perceptual distances on a palette-function. (#38)

* Added functions `data_surface_hl()`, `plot_surface_hl()` that work with hue-luminance surface functions. (#31)

* Added functions `surface_hl() ` and `df_hcl()` to facilitate creating sets of consistent multi-hue palettes. (#29)

* Added functions `pal_luv_rescale_x()` and `pal_luv_rescale_lum()` to avoid creating rescaler functions explicitly. (#27)

* Added pkgdown site. (#22)

* Added `print()` method for `cpath_pal_luv`.

* Added function `plot_cl()` to plot chroma-lumuinance plane for a palette. (#15)

* Added argument `chroma_min` (default 0.01) to `as_mat_luv()` to keep meaningful hue at low chroma. (#15)

* Added function `data_hcl()` to get HCL data to visualize palettes. (#15)

* Refactored to add constuctor for LUV palettes, store `mat_luv` control-points as attribute. (#19)

* Refactored (#1, continued):

  - added arguments `rescale_path` and `n` to `pal_luv_bezier()`, rescales the palette to be perceptually uniform (in LUV space) with respect to the input. 
  - made `rescaler_bezier()` an internal function.
  - renamed function `palette_bezier()` to `pal_luv_bezier()`; added argument 
    `rescale_path`, defaults `TRUE`, to indicate to rescale the input according 
    to the path length in LUV space.
  - reanmed function `rescale_palette()` to `rescale_pal_luv()`.
  - renamed function `rescaler_linear_luminance()` to `rescaler_lum()`.
  - renamed function `rescaler_linear_input()` to `rescaler_x()`.
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
