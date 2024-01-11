#' @title GRadient-based RECognition of spatial patterns in environmental data
#' @description Provides algorithms for detecting spatial patterns from 2-D
#' oceanographic data using image processing methods based on Gradient
#' Recognition.
#' @keywords internal
"_PACKAGE"

## usethis namespace: start
#' @importFrom abind abind
#' @importFrom imagine contextualMF convolution2D medianFilter agenbagFilters
#' @importFrom lifecycle deprecate_soft deprecate_warn badge
#' @importFrom lifecycle deprecated
#' @importFrom raster raster values nlayers
#' @importFrom terra rast values nlyr crs ext varnames origin
#' @importFrom utils modifyList
#' @importMethodsFrom raster "["
#' @importMethodsFrom terra as.matrix varnames<- origin<- ext<-
## usethis namespace: end
NULL
