#' @rdname detectFronts
#' @method detectFronts RasterLayer
#' @export
detectFronts.RasterLayer <- function(x, qLimits = c(0.9, 0.99), finalSmooth = FALSE, intermediate = FALSE,
                                control = list()){

  # Extract coordinates and data for calculate fronts from Raster and convert to list
  startMatrix <- matrix(data = x@data@values, nrow = x@ncols, ncol = x@nrows)
  startMatrix <- list(x = seq(from = x@extent@xmin, to = x@extent@xmax, length.out = x@ncols),
                      y = seq(from = x@extent@ymin, to = x@extent@ymax, length.out = x@nrows),
                      z = startMatrix)
                      # z = startMatrix[,seq(x@nrows, 1)])

  # Apply the basic function
  allOuts <- detectFronts.default(x = startMatrix, qLimits = qLimits, finalSmooth = finalSmooth,
                                  intermediate = intermediate, control = control)

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
