#' @rdname detectFronts
#' @method detectFronts RasterLayer
#' @export
detectFronts.RasterLayer <- function(x, method = "BelkinOReilly2009", intermediate = FALSE, ...){

  checkArgs_df_RasterLayer(x = x)

  # Extract coordinates and data for calculate fronts from Raster and convert to list
  startMatrix <- values(x)
  startMatrix <- list(x = seq(from = x@extent@xmin, to = x@extent@xmax, length.out = x@ncols),
                      y = seq(from = x@extent@ymin, to = x@extent@ymax, length.out = x@nrows),
                      z = matrix(data = startMatrix, nrow = x@ncols))

  allOuts <- detectFronts(x = startMatrix, method = method, intermediate = intermediate, ...)

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
