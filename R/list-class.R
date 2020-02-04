#' @rdname detectFronts
#' @method detectFronts list
#' @export
detectFronts.list <- function(x, method = "BelkinOReilly2009", intermediate = FALSE, ...){

  # Check arguments
  checkArgs_df_list(x = x)

  # Apply method
  if(isTRUE(intermediate)){
    output <- list()

    for(i in seq(dim(x$z)[3])){
      tempOut <- x
      tempOut$z <- detectFronts(x = x$z[,,i], method = method, intermediate = intermediate, ...)

      output[[i]] <- tempOut
    }
  }else{
    output <- x
    output$z <- detectFronts(x = x$z, method = method, intermediate = intermediate, ...)
  }

  return(output)
}
