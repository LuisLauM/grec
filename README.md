grec
=======

[![packageversion](https://img.shields.io/badge/Package%20version-`1.4.1`-orange.svg?style=flat-square)](commits/master) [![CRAN_Status_Badge](https://www.r-pkg.org/badges/version/grec)](https://cran.r-project.org/package=grec) [![CRAN_time_from_release](https://www.r-pkg.org/badges/ago/grec)](https://cran.r-project.org/package=grec) [![metacran downloads](https://cranlogs.r-pkg.org/badges/grec)](https://cran.r-project.org/package=grec) [![minimal R version](https://img.shields.io/badge/R%3E%3D-`3.2.0`-6666ff.svg)](https://cran.r-project.org/)

**[G]radient-based [REC]ognition of Spatial Patterns in Environmental Data**

Provides algorithms for detection of spatial patterns from oceanographic data using image processing methods based on Gradient Recognition.

Installation
------------

Get the development version from github:

``` r
# install.packages("devtools")
devtools::install_github("LuisLauM/grec")
```

Or install the CRAN version

``` r
install.packages("grec")
```

Examples
--------

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
