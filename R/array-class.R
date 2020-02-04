#' @rdname detectFronts
#' @method detectFronts array
#' @export
detectFronts.array <- function(x, method = "BelkinOReilly2009", intermediate = FALSE, ...){

  # Check arguments
  checkArgs_df_array(x = x)

  output <- if(intermediate) list() else x
  for(i in seq(dim(x)[3])){
    # Apply the basic function
    tempOut <- detectFronts(x = x[,,i], method = method, intermediate = intermediate, ...)

    # Depending on 'intermediate', the output will be a single array or a list of them
    if(intermediate){
      output[[i]] <- tempOut
    }else{
      output[,,i] <- tempOut
    }
  }

  return(output)
}
