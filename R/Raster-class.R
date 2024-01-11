#' @rdname getGradients
#' @method getGradients RasterLayer
#' @export
getGradients.RasterLayer <- function(x, method = "BelkinOReilly2009",
                                     intermediate = FALSE, ...){

  deprecate_warn(when = "as soon as raster package is removed from CRAN",
                 what = I("raster::RasterLayer method"),
                 with = I("terra::SpatRaster method"))
  checkArgs_df_RasterLayer(x = x)

  # Extract coordinates and data for calculate fronts from Raster and convert to list
  startMatrix <- values(x)
  startMatrix <- list(x = seq(from = x@extent@xmin, to = x@extent@xmax, length.out = x@ncols),
                      y = seq(from = x@extent@ymin, to = x@extent@ymax, length.out = x@nrows),
                      z = matrix(data = startMatrix, nrow = x@ncols))

  allOuts <-  getGradients(x = startMatrix, method = method, intermediate = intermediate,
                           checkPrevs = FALSE, ...)

  # Depending on 'intermediate', the output will be a single Raster or a list of them
  if(intermediate){
    allSteps <- dimnames(allOuts$z)[[3]]

    output <- list()
    for(i in seq_along(allSteps)){
      tempOut <- x
      tempOut[] <- as.numeric(allOuts$z[,,i])
      output[[i]] <- tempOut
    }

    names(output) <- allSteps
  }else{
    output <- x
    output[] <- as.numeric(allOuts$z)
  }

  output
}
