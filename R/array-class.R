#' @rdname detectFronts
#' @method detectFronts array
#' @export
detectFronts.array <- function(x, method = "BelkinOReilly2009", intermediate = FALSE, ...){

  # Check arguments
  checkArgs_df_array(x = x)

  # output <- if(intermediate) list() else x
  for(i in seq(dim(x)[3])){
    # Apply the basic function
    tempOut <- detectFronts(x = x[,,i], method = method, intermediate = intermediate,
                            checkPrevs = FALSE, ...)

    # Depending on 'intermediate', the output will be a single array or a list of them
    if(intermediate){
      if(i == 1){
        output <- array(data = NA, dim = c(dim(x), dim(tempOut)[3]))
      }

      output[,,i,] <- tempOut
    }else{
      output[,,i] <- tempOut
    }
  }

  return(output)
}
