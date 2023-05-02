#' @rdname detectFronts
#' @method detectFronts SpatRaster
#' @export
detectFronts.SpatRaster <- function(x, method = "BelkinOReilly2009",
                                    intermediate = FALSE, ...){

  checkArgs_df_SpatRaster(x = x)

  y <- as.matrix(x = x, wide = TRUE)

  if(nlyr(x) > 1){
    index <- rep(x = seq(nlyr(x)), each = ncol(y)/nlyr(x))

    y <- lapply(seq(nlyr(x)), function(x) y[,is.element(index, x)])

    y <- abind(y, along = 3)
  }

  y <- rast(x = detectFronts(x = y,
                             method = method,
                             intermediate = FALSE,
                             checkPrevs = FALSE, ...),
            crs = crs(x), extent = ext(x))

  names(y) <- sprintf("gradient.%s.%04d", varnames(x), seq(nlyr(x)))
  origin(y) <- origin(x)
  varnames(y) <- sprintf("gradient of %s", varnames(x))

  y
}
