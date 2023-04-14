#' @title GRadient-based RECognition of spatial patterns in environmental data
#' @importFrom imagine contextualMF convolution2D medianFilter
#' @importFrom terra values
#' @importFrom raster values
#' @importFrom utils modifyList
#'
#' @author Wencheng Lau-Medrano, \email{luis.laum@gmail.com}
#' @name grec-package
#' @description Provides algorithms for detection of spatial patterns from oceanographic
#' data using image processing methods based on Gradient Recognition.
#' @aliases grec-package grec
#' @docType package
#' @concept gradient
#' @concept pattern-detection
#' @concept environmental-data
NULL

#' @title Default color palette most using on environmental representations.
#' @name colPalette
#' @description Vector with 2000 colors generated from \code{tim.colors} function.
#' @aliases colPalette
#' @docType data
#' @usage colPalette
#' @format A vector of 2000 colors in RGB format.
#' @references \code{tim.colors} from \strong{fields} package
NULL

#' @title Sea Surface Temperature Data
#' @name sst
#' @description SST maps downloaded from ERDDAP for running examples with \code{grec}
#' functions.
#' @aliases sst
#' @docType data
#' @usage sst
#' @format A \code{list} with SST information from February to April of Aqua MODIS source.
#' @references ERDDAP website: \url{https://coastwatch.pfeg.noaa.gov/erddap/index.html}
NULL

#' @title Sea Surface Chlorophyll Data
#' @name chl
#' @description Surface chlorophyll maps downloaded from ERDDAP for running examples with
#' \code{grec} functions.
#' @aliases chl
#' @docType data
#' @usage chl
#' @format A \code{list} with chlorophyll information from February to April of Aqua MODIS
#' source.
#' @references ERDDAP website: \url{https://coastwatch.pfeg.noaa.gov/erddap/index.html}
NULL

#' @title Apply gradient-based methodologies to environmental data
#'
#' @description This function takes a environmental map (as a numeric \code{matrix}, \code{array},
#' XYZ\code{list} or \code{RasterLayer}) and allows the users to apply methodologies based on
#' gradient-searching.
#'
#' @rdname detectFronts
#'
#' @param x Main input of class \code{matrix}, \code{array}, XYZ \code{list} or \code{RasterLayer}.
#' See 'Details.'
#' @param method \code{character} string indicating the method that will be used. See 'Details'.
#' @param intermediate \code{logical} indicating whether to get the intermediate matrices
#' (\code{TRUE}) or just the final one (\code{FALSE}).
#' @param ConvolNormalization \code{logical} indicating if convolutions will make a normalization
#' (\code{TRUE} by default). See Details.
#' @param ... Extra arguments that will depend on the selected method. See Details.
#'
#' @details Version 1.3.x performs two methods:
#' \enumerate{
#' \item \code{BelkinOReilly2009} (default): Based on Belkin & O'Reilly (2009) paper, it uses a
#' Contextual Median Filter (CMF) for smoothing the original data.
#' \item \code{median_filter}: it uses a typical median filter (MF) for smoothing the original
#' data. It also allows the user to change the window size for median filter (3 as default).
#' }
#'
#' \code{x} could be given as a single numeric \code{matrix} from an environmental map. Othersiwe
#' it also can be set as a three-dimension XYZ \code{list}: 'x' (a vector of longitudes), 'y'
#' (vector of latitudes) and 'z' as a matrix of dimensions \code{length(x$x)}x\code{xlength(x$y)}.
#' You can also specify \code{x} as a \code{RasterLayer} or \code{array} object. If \code{x} is an
#' \code{array}, it must have 3 dimensions: lon, lat and time. It is not required to specify the
#' \code{dimnames}.
#' The output will preserve all the attributes of input.
#'
#' \code{...} allows the (advanced) users to modify some aspects of filter application. Depending
#' on the selected methodology, some parameters can be modified:
#'
#' \describe{
#' \item{\strong{times}}{\code{numeric}. How many times do you want to apply the method?}
#' \item{\strong{kernelValues}}{\code{numeric}. Vector with which are going to be used in
#' convolution to identify Vertical and Horizontal gradients. By default, it will be the typical
#' Sobel kernels.}
#' \item{\strong{radius}}{\code{numeric}. If median filter method was selected, it allows to change
#' the window size of the filter.}
#' }
#'
#' Normalization is a common practice in convolution in order to ensure that outputs are weighted
#' within original range of values. It is achieved dividing outputs of convolution by sum(abs(kernel)).
#' It is hardly recomended to use normalization in order to have always coherent values in regards of
#' the original inputs; however, it can be deactivated by using \code{ConvolNormalization} argument.
#'
#' Finally, Belkin & O'Reilly work proposed a log transformation after the gradient calculation.
#' However, this step has not been considered as default in the function due to its application
#' is focused on Chlorophyll values (maps).
#'
#' @references Belkin, I. M., & O'Reilly, J. E. (2009). An algorithm for oceanic front detection
#' in chlorophyll and SST satellite imagery. Journal of Marine Systems, 78(3), 319-326
#' (\url{http://dx.doi.org/10.1016/j.jmarsys.2008.11.018}).
#'
#' @return The output will preserve the input class (\code{matrix}, \code{array}, \code{list} or
#' \code{RasterLayer}).
#'
#' @export
#'
#' @examples
#' data(sst)
#' exampleSSTData <- list(x = sst$longitude,
#'                        y = sst$latitude,
#'                        z = sst$sst[,,1])
#'
#' data(chl)
#' exampleChlData <- list(x = chl$longitude,
#'                        y = chl$latitude,
#'                        z = chl$chlorophyll[,,1])
#'
#' # Simple application (over a XYZ list)
#' out_sst <- detectFronts(x = exampleSSTData)
#' out_chl <- detectFronts(x = exampleChlData)
#'
#' # External transformation for chl data
#' out_chl$z <- log10(out_chl$z)
#'
#' par(mfrow = c(2, 2), mar = rep(0, 4), oma = rep(0, 4))
#'
#' image(exampleSSTData, col = colPalette, axes = FALSE)
#' mtext(text = "Original SST", side = 3, line = -2, adj = 0.99, cex = 1.2)
#'
#' image(out_sst, col = colPalette, axes = FALSE)
#' mtext(text = "SST gradient", side = 3, line = -2, adj = 0.99, cex = 1.2)
#'
#' image(exampleChlData, col = colPalette, axes = FALSE)
#' mtext(text = "Original Chlorophyll", side = 3, line = -2, adj = 0.99, cex = 1.2)
#'
#' image(out_chl, col = colPalette, axes = FALSE)
#' mtext(text = "Chlorophyll gradient\n(log scale)", side = 3, line = -4, adj = 0.99,
#'       cex = 1.2)
detectFronts <- function(x, method = "BelkinOReilly2009", intermediate = FALSE, ConvolNormalization = TRUE, ...){

  checkPrevs <- list(...)$checkPrevs
  if(is.null(checkPrevs) || checkPrevs){
    # Check and validation of arguments
    checkedArgs <- list(method = method, intermediate = intermediate, ConvolNormalization = ConvolNormalization, ...)
    checkArgs_prevs(allArgs = checkedArgs, type = class(x))
  }

  UseMethod(generic = "detectFronts", object = x)
}
