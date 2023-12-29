#' @title GRadient-based RECognition of spatial patterns in environmental data
#' @description Provides algorithms for detecting spatial patterns from 2-D
#' oceanographic data using image processing methods based on Gradient
#' Recognition.
#' @keywords internal
"_PACKAGE"

## usethis namespace: start
#' @importFrom imagine contextualMF convolution2D medianFilter agenbagFilters
#' @importFrom terra rast values nlyr crs ext varnames origin
#' @importFrom raster raster values nlayers
#' @importFrom utils modifyList
#' @importFrom abind abind
#' @importFrom lifecycle deprecate_soft deprecate_warn
#' @importMethodsFrom terra as.matrix varnames<- origin<- ext<-
#' @importMethodsFrom raster "["
## usethis namespace: end
NULL
