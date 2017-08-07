#' @title An R package for GRadient-based RECognition of spatial patterns in environmental data
#' @import imagine
#' @import raster
#' @importFrom utils modifyList
#'
#' @author Wencheng Lau-Medrano, \email{luis.laum@gmail.com}
#' @name grec-package
#' @description Provides algorithms for detection of spatial patterns from oceanographic data using image
#' processing methods based on Gradient Recognition.
#' @aliases grec-package grec
#' @docType package
#' @keywords gradient, pattern-detection, environmental-data
NULL

#' @title Default color palette most using on environmental representations.
#' @name colPalette
#' @description Vector with 2000 colors generated from \code{tim.colors} function.
#' @aliases colPalette
#' @docType data
#' @usage colPalette
#' @format A vector of 2000 colors in RBG format.
#' @references \code{fields} package
NULL

#' @title Example map of SST from Aqua MODIS
#' @name grecExData
#' @description A \code{list} containing three environmental dataset for using as examples. Datasets
#' of Surface Sea Temperature (\code{sst}), Chlorophyll at surface (\code{chlorophyll}) and Bathymetry
#' (topography) were dowloaded from ERDDAP website.
#'
#' Chlorophyll and SST data are XYZT lists with monthly info from February to April 2015 from source
#' Aqua MODIS, March, 2010.
#' @aliases exampleSSTData
#' @docType data
#' @usage exampleSSTData
#' @format A \code{list} with 3 levels (x, y, z). \code{x} and \code{y} are numeric vectors, \code{z}
#' is a numeric \code{matrix}.
#' @references ERDDAP website: \url{https://coastwatch.pfeg.noaa.gov/erddap/index.html}
NULL

#' @title Detection of fronts based on gradient recognition
#'
#' @description This function takes a environmental map (as a numeric matrix) and allows the user to idenitify
#' the gradients by using of sobel filters.
#'
#' @rdname detectFronts
#'
#' @param x Main input of class \code{matrix}, \code{list}, \code{RasterLayer} or \code{array}. See 'Details.'
#' @param qLimits \code{numeric} vector of length 1 or 2 with info of limits of values to consider. See 'Details'.
#' @param finalSmooth \code{logical} indicating whether to apply a smooth to final matrix so as to remove noise.
#' @param intermediate \code{logical} indicating whether to get the intermediate matrices (\code{TRUE})
#' or just the final one (\code{FALSE}).
#' @param control A \code{list} of control parameters for filter application See 'Details'.
#'
#' @details Inspired by the algorithm described on Belkin & O'Reilly (2009), this function performs 4 steps:
#' \enumerate{
#' \item Smoothing of the original data by a median filter application.
#' \item Application of sobel filters horizontally (sobelH) and vertically (sobelV).
#' \item Extract gradients, using the formula \eqn{sqrt(sobelH^2 + sobelV^2)}.
#' \item Removing noise signals using a median filter, from \code{imagine} package.
#' }
#'
#' \code{x} could be given as a single numeric matrix containing the values of a
#' environmental map. Othersiwe it also can be a list with dimensions 'x', 'y' and 'z' specifying
#' the dimensions of the data as follows: 'x' will be a numeric vector with the values of longitude,
#' 'y' will indicate the latitude (numeric vector as well). 'grec' package is not rigorous in
#' the check of the values given for dimensions, so the user must be carefull with them.
#'
#' \code{x} can be specified as a \code{RasterLayer} or \code{array} object. If \code{x} is an \code{array}, it
#' must have 3 dimensions: lon, lat and time. It is not required to specify the \code{dimnames}. The output will
#' preserve all the attributes and the order of input.
#'
#' \code{qLimits} works after the extraction of grandient matrix. Values of these matrix are vectorized
#' and the quantiles indicated on \code{qLimits} are taken (that is the reason of the argument name). Then
#' the values out of the limits are replaced by \code{NA}. \code{qLimits} could be given as a single value.
#' If so, the second value must be calculated as \code{c(qLimits, qLimits + (1 - qLimits)/2)}.
#'
#' The control argument is a list that allows the (advanced) users modify some aspects of filter
#' application. The parameters of \code{control} are given to functions of \code{\link{imagine}} package.
#' It must be a \code{list} including the following named objects:
#' \describe{
#' \item{\strong{firstSmooth}}{Arguments (\code{radius} and \code{times}) pased to \code{\link{medianFilter}}
#' function, used for apply the smoothing to the original matrix. It must be given as a named list.}
#' \item{\strong{sobelStrength}}{Number that multiplies \code{qLimits} vector. It is usefull to highlight
#' the differences.}
#' \item{\strong{clearNoise}}{Arguments (\code{radius} and \code{times}) pased to \code{\link{medianFilter}}
#' function, used for apply the median-filter for cleaning noise and getting the output matrix. It must be
#' given as a named list.}
#' }
#'
#' @references Belkin, I. M., & O'Reilly, J. E. (2009). An algorithm for oceanic front detection in chlorophyll
#' and SST satellite imagery. Journal of Marine Systems, 78(3), 319-326
#' (\url{http://dx.doi.org/10.1016/j.jmarsys.2008.11.018}).
#'
#' @return Depending of input class of \code{x}, the output will preserve its class.
#'
#' @export
#'
#' @examples
#' # Build an example data
#' # Load example data
#' exampleSSTData <- list(x = grecExData$sst$longitude,
#'                        y = grecExData$sst$latitude,
#'                        z = grecExData$sst$sst[,,1])
#'
#' out <- detectFronts(x = exampleSSTData)
#' image(out, col = colPalette)
detectFronts <- function(x, qLimits = c(0.9, 0.99), finalSmooth = FALSE, intermediate = FALSE, control = list()){
  UseMethod(generic = "detectFronts", object = x)
}

#' @title Gets the extra parameters for \code{grec} functions
#'
#' @description Show as a list the extra parameters for main \code{grec} function.
#'
#' @param fx \code{character} vector indicating the values that will be returned.
#'
#' @details This function is usefull for advance users that require to modify intermediate steps in terms of
#' filter application.
#'
#' @return A \code{list} with the extra parameters used by default.
#' @export
#'
#' @examples
#' # For getting all the extra parameters
#' extraParams()
extraParams <- function(fx = c("detectFronts")){
  # Check and validation of arguments
  checkedArgs <- list(fx = fx)
  checkedArgs <- checkArgs(grecArgs = checkedArgs, type = as.character(match.call())[1])

  output <- with(checkedArgs, extraParams_internal(fx = fx))

  return(output)
}
