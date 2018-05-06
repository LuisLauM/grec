#' @title GRadient-based RECognition of spatial patterns in environmental data
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
#' @format A vector of 2000 colors in RGB format.
#' @references \code{fields} package
NULL

#' @title Sea Surface Temperature Data
#' @name sst
#' @description SST maps downloaded from ERDDAP for running examples with \code{grec} functions.
#' @aliases sst
#' @docType data
#' @usage sst
#' @format A \code{list} with SST information from February to April of Aqua MODIS source.
#' @references ERDDAP website: \url{https://coastwatch.pfeg.noaa.gov/erddap/index.html}
NULL

#' @title Sea Surface Chlorophyll Data
#' @name chl
#' @description Surface chlorophyll maps downloaded from ERDDAP for running examples with \code{grec} functions.
#' @aliases chl
#' @docType data
#' @usage chl
#' @format A \code{list} with chlorophyll information from February to April of Aqua MODIS source.
#' @references ERDDAP website: \url{https://coastwatch.pfeg.noaa.gov/erddap/index.html}
NULL

#' @title Bathymetric data
#' @name bathy
#' @description Bathymetric maps downloaded from ERDDAP for running examples with \code{grec} functions.
#' @aliases bathy
#' @docType data
#' @usage bathy
#' @format A \code{list} with bathymetric information from ETOPO source.
#' @references ERDDAP website: \url{https://coastwatch.pfeg.noaa.gov/erddap/index.html}
NULL

#' @title Apply gradient-based methodologies to environmental data
#'
#' @description This function takes a environmental map (as a numeric \code{matrix}, \code{array}, XYZ\code{list}
#' or \code{RasterLayer}) and allows the users to apply methodologies based on gradient-searching.
#'
#' @rdname detectFronts
#'
#' @param x Main input of class \code{matrix}, \code{array}, XYZ \code{list} or \code{RasterLayer}. See 'Details.'
#' @param method \code{character} string indicating the method that will be used. See 'Details'.
#' @param intermediate \code{logical} indicating whether to get the intermediate matrices (\code{TRUE})
#' or just the final one (\code{FALSE}).
#' @param control A \code{list} of control parameters for filter application See 'Details'.
#' @param ... Extra arguments that will depend on the selected method.
#'
#' @details Version 1.2.x performs one method: Belkin & O'Reilly (2009), following 3 steps:
#' \enumerate{
#' \item Apply a Contextual Median Filter (CMF) for smoothing the original data.
#' \item Apply a convolution with sobel kernels horizontally (sobelH) and vertically (sobelV).
#' \item Extract gradients using the formula \eqn{sqrt(sobelH^2 + sobelV^2)}.
#' }
#'
#' \code{x} could be given as a single numeric \code{matrix} from an environmental map. Othersiwe it also can be set
#' as a three-dimension XYZ \code{list}: 'x' (a vector of longitudes), 'y' (vector of latitudes) and
#' 'z' as a matrix of dimensions \code{length(x$x)}x\code{xlength(x$y)}. You can also specify \code{x} as a
#' \code{RasterLayer} or \code{array} object. If \code{x} is an \code{array}, it must have 3 dimensions: lon, lat
#' and time. It is not required to specify the \code{dimnames}. The output will preserve all the attributes of input.
#'
#' Users can change the methodology used for the calculation of gradients by \code{method}. By default it will be
#' the Belkin & O'Reilly (2009) at v1.2.x.
#'
#' \code{...} allows the (advanced) users to modify some aspects of filter application. Depending on the selected methodology,
#' the available arguments will change. So, Belkin & O'Reilly (2009) brings out the following arguments to change:
#'
#' \describe{
#' \item{\strong{inner_radius}}{\code{numeric}. Size (in pixels) of window for the first stage on CMF.}
#' \item{\strong{outer_radius}}{\code{numeric}. Size (in pixels) of window for the second stage on CMF.}
#' \item{\strong{times}}{\code{numeric}. How many times do you want to apply the CMF?}
#' \item{\strong{kernelValues}}{\code{numeric}. Vector with which are going to be used in convolution to identify Vertical
#' and Horizontal gradients. By default, it will be the typical Sobel kernels.}
#' }
#'
#' @references Belkin, I. M., & O'Reilly, J. E. (2009). An algorithm for oceanic front detection in chlorophyll
#' and SST satellite imagery. Journal of Marine Systems, 78(3), 319-326
#' (\url{http://dx.doi.org/10.1016/j.jmarsys.2008.11.018}).
#'
#' @return The output will preserve the input class (\code{matrix}, \code{array}, \code{list} or \code{RasterLayer}).
#'
#' @export
#'
#' @examples
#' # Build an example data
#' # Load example data
#' data(sst)
#' exampleSSTData <- list(x = sst$longitude,
#'                        y = sst$latitude,
#'                        z = sst$sst[,,1])
#' # Simple application (over a XYZ list)
#' out <- detectFronts(x = exampleSSTData)
#' image(out, col = colPalette)
detectFronts <- function(x, method = "BelkinOReilly2009", intermediate = FALSE, ...){
  # Check and validation of arguments
  checkedArgs <- list(x = x, method = method, intermediate = intermediate, ...)
  checkArgs(allArgs = checkedArgs, type = class(x))

  UseMethod(generic = "detectFronts", object = x)
}
