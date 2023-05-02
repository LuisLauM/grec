grec
=======

[![packageversion](https://img.shields.io/badge/Package%20version-1.4.1-orange.svg?style=flat-square)](commits/master) [![CRAN_Status_Badge](https://www.r-pkg.org/badges/version/grec)](https://cran.r-project.org/package=grec) [![CRAN_time_from_release](https://www.r-pkg.org/badges/ago/grec)](https://cran.r-project.org/package=grec) [![metacran downloads](https://cranlogs.r-pkg.org/badges/grec)](https://cran.r-project.org/package=grec) [![minimal R version](https://img.shields.io/badge/R%3E%3D-3.2.0-6666ff.svg)](https://cran.r-project.org/)

**[G]radient-based [REC]ognition of Spatial Patterns in Environmental Data**

Provides algorithms for detection of spatial patterns from oceanographic data using image processing methods based on Gradient Recognition.

## Installation

Get the development version from github:

``` r
# install.packages("devtools")
devtools::install_github("LuisLauM/grec")
```

Or install the CRAN version

``` r
install.packages("grec")
```

## Examples

Next, we show an example of the use of `detectFronts` function:

``` r
require(grec)

data(sst)
exampleSSTData <- list(x = sst$longitude,
                       y = sst$latitude,
                       z = sst$sst[,,1])

data(chl)
exampleChlData <- list(x = chl$longitude,
                       y = chl$latitude,
                       z = chl$chlorophyll[,,1])

# Simple application (over a XYZ list)
out_sst <- detectFronts(x = exampleSSTData)
out_chl <- detectFronts(x = exampleChlData)

# External transformation for chl data
out_chl$z <- log10(out_chl$z)

par(mfrow = c(2, 2), mar = rep(0, 4), oma = rep(0, 4))

image(exampleSSTData, col = colPalette, axes = FALSE)
mtext(text = "Original SST", side = 3, line = -2, adj = 0.99, cex = 1.2)

image(out_sst, col = colPalette, axes = FALSE)
mtext(text = "SST gradient", side = 3, line = -2, adj = 0.99, cex = 1.2)

image(exampleChlData, col = colPalette, axes = FALSE)
mtext(text = "Original Chlorophyll", side = 3, line = -2, adj = 0.99, cex = 1.2)

image(out_chl, col = colPalette, axes = FALSE)
mtext(text = "Chlorophyll gradient\n(log scale)", side = 3, line = -4, adj = 0.99,
      cex = 1.2)
```

## Versions

### 1.3.5
  - Corrections in Belkin and O'Reilly method (GX and GY kernel orientation).
  - Adding methods for `SpatRast` (from **terra** package) objects.
  - Support for **Raster-** objects will be retired since next versions of **grec**.
  - Improving methods for `array` objects.
  - `ConvolNormalization` argument is now set as `FALSE` by default in `detectFronts` function.
  - Minor corrections and improvements in efficiency.

### 1.3.4
  - Improvements on the way to work with matrix, arrays, list and RasterLayer objects.
  - Correcting some bugs regarded with methods.
  - Include `ConvolNormalization` argument and enable normalization of convolutions as default.
  - Add documentation of methods.

### 1.2.2
  - Improvements on the way to extract matrix data from RasterLayer object.
  - Correcting a bug when RasterLayer has not in-memory loaded data (e.g. RasterLayer gotten from read a tif file).
  - Add documentation for NW_USA_SST data.
  
### 1.2.1
  - Some corrections in demos.
  - Include a new example data set for SST: NW_USA_SST
  - Testing normalization inside B&O method.

### 1.2.0
  - Several changes and improvments on package structure.
  - `finalSmooth` and `control` arguments `detectFronts` were deleted.
  - `extraParams` function were deleted. Now, available advanced arguments will be showed in help.
  - Help document for `detectFronts` were updated.
  - Another little corrections and improvements.

### 1.1.2
  - Now, `detectFronts` function has the `method` argument, using by default the Belkin & O'Reilly (2009) algorithm.
  - Some little corrections.

### 1.1.1
  - Modifications on the title of package.
  - New set of exampling data (`grecExData`).
  - Changes how the `detectFronts` algorithm works internally.
  
### 1.1.0
  - Change function name: from `fromDetection` to `detectFronts`.
  - Change several argument names on `detectFronts` function.
  - `thresholds` argument has been replaced by `qLimits`. Check help of `detectFronts` function for details.
  - On `detectFronts` function, the final smooth application is optional now, controling by `finalSmooth` argument.
  - Now `detectFronts` is a method for classes: `matrix`, `array` and `RasterLayer`.
  - Bugs and corrections.

### 1.0.0
  - First Public Release
