#' @rdname detectFronts
#' @method detectFronts RasterLayer
#' @export
detectFronts.RasterLayer <- function(x, method = "BelkinOReilly2009", finalSmooth = FALSE,
                                     intermediate = FALSE, control = list(), ...){

  # Extract coordinates and data for calculate fronts from Raster and convert to list
  startMatrix <- matrix(data = x@data@values, nrow = x@ncols, ncol = x@nrows)
  startMatrix <- list(x = seq(from = x@extent@xmin, to = x@extent@xmax, length.out = x@ncols),
                      y = seq(from = x@extent@ymin, to = x@extent@ymax, length.out = x@nrows),
                      z = startMatrix)

  # Check and validation of arguments
  checkedArgs <- list(x = startMatrix, method = method, finalSmooth = finalSmooth, intermediate = intermediate,
                      control = control, ...)
  checkedArgs <- checkArgs(grecArgs = checkedArgs, type = as.character(match.call())[1])

  # Apply the basic function
  allOuts <- with(checkedArgs,
                  switch(method,
                         BelkinOReilly2009 = detectFronts_BelkinOReilly2009(x = startMatrix$z,
                                                                            finalSmooth = finalSmooth,
                                                                            intermediate = intermediate,
                                                                            control = control),
                         LauMedrano = detectFronts_LauMedrano(x = startMatrix$z,
                                                              qLimits = list(...)$qLimits,
                                                              finalSmooth = finalSmooth,
                                                              intermediate = intermediate,
                                                              control = control)))

  # Depending on 'intermediate', the output will be a single Raster or a list of them
  if(isTRUE(intermediate)){
    output <- list()
    for(i in seq_along(allOuts)){
      tempOut <- x
      tempOut[] <- as.numeric(allOuts[[i]]$z)
      output[[i]] <- tempOut
    }

    names(output) <- names(allOuts)
  }else{
    output <- x
    output[] <- as.numeric(allOuts$z)
  }

  return(output)
}
