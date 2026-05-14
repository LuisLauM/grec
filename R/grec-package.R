#' @title GRadient-based RECognition of spatial patterns in environmental data
#' @description Provides algorithms for detecting spatial patterns from 2-D
#' oceanographic data using image processing methods based on Gradient
#' Recognition.
#' @keywords internal
"_PACKAGE"

## usethis namespace: start
#' @importFrom abind abind
#' @importFrom cli cli_abort
#' @importFrom imagine agenbagFilters
#' @importFrom imagine contextualMF
#' @importFrom imagine convolution2D
#' @importFrom imagine medianFilter
#' @importFrom lifecycle badge
#' @importFrom lifecycle deprecate_soft
#' @importFrom lifecycle deprecate_warn
#' @importFrom lifecycle deprecated
#' @importFrom raster nlayers
#' @importFrom raster raster
#' @importFrom raster values
#' @importFrom terra 'values<-'
#' @importFrom terra as.array
#' @importFrom terra crs
#' @importFrom terra ext
#' @importFrom terra nlyr
#' @importFrom terra origin
#' @importFrom terra rast
#' @importFrom terra values
#' @importFrom terra varnames
#' @importFrom utils modifyList
#' @importMethodsFrom raster "["
#' @importMethodsFrom terra as.matrix
#' @importMethodsFrom terra ext<-
#' @importMethodsFrom terra origin<-
#' @importMethodsFrom terra varnames<-
## usethis namespace: end
NULL
