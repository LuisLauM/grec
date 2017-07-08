#' @title Classification of spatial patterns from environmental data through GRadient RECognition
#' @import imagine
#'
#' @author Wencheng Lau-Medrano, \email{luis.laum@gmail.com}
#' @name grec-package
#' @description It provides algorithms for detection of spatial patterns from oceanographic data.
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

#' @title Detection of fronts based on gradient recognition
#'
#' @description This function takes a environmental map (as a numeric matrix) and allows the user to idenitify
#' the gradients by using of sobel filters.
#'
#' @param envirData Either a list o numerical matrix with environmental info. See Details.
#' @param thresholds \code{numeric} vector of length 1 or 2 with info of limits of values to consider. See Details.
#' @param stepByStep \code{logical} indicating whether to get the intermediate matrices (\code{TRUE})
#' or just the final one (\code{FALSE}).
#'
#' @details That function performs 4 steps:
#' \enumerate{
#' \item Smoothing of the original data.
#' \item Application of sobel filters horizontally (sobelH) and vertically (sobelV).
#' \item Extract gradients, using the formula \eqn{sqrt(sobelH^2 + sobelV^2)}.
#' \item Removing noise signals using a median filter, from \code{imagine} package.
#' }
#'
#' In order to improve the extraction of fronts, the package allows users to change the \code{thresholds}
#' values. It is better detailed at vignettes.
#'
#' \code{envirData} could be given as a single numeric matrix containing the values of a
#' environmental map. Othersiwe it also can be a list with dimensions 'x', 'y' and 'z' specifying
#' the dimensions of the data as folloews: 'x' will be a numeric vector with the values of longitude,
#' 'y' will indicate the latitude (numeric vector as well). 'grec' package does not be rigorous in
#' the check of the values given for dimensions, so the user must be carefull with them.
#'
#' \code{thresholds} could be given as a single value. If so, the second value must be calculated
#' as a 5 times the given value. That argument must be applied after the smoothing (step 1).
#'
#' @return Depending on \code{stepByStep} argument, it can be a list or a single numeric matrix.
#' @export
#'
#' @examples
#' load(system.file("extdata", "exampleSSTData.RData", package = "grec"))
#' out <- frontDetect(envirData = exampleSSTData, threshold = c(10, 500), stepByStep = FALSE)
#' image(out, col = colPalette)
frontDetect <- function(envirData, thresholds, stepByStep = TRUE){
  # Check and validation of arguments
  checkedArgs <- list(envirData = envirData, thresholds = thresholds, stepByStep = stepByStep)
  checkedArgs <- checkArgs(grecArgs = checkedArgs, type = as.character(match.call())[1])

  # Define extra parameters for filter
  extraParams <- list(firstSmooth = list(radius = 5,
                                         times = 10),
                      sobelStrength = 10,
                      clearNoise = list(radius = 5,
                                        times = 1))

  # Apply filters
  output <- with(checkedArgs,
                 frontDetect_internal(envirData = envirData, thresholds = thresholds, stepByStep = stepByStep,
                                      extraParams = extraParams))

  return(output)
}
