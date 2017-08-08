#' @rdname detectFronts
#' @method detectFronts array
#' @export
detectFronts.array <- function(x, qLimits = c(0.9, 0.99), finalSmooth = FALSE, intermediate = FALSE,
                               control = list()){

  # Check and validation of arguments
  checkedArgs <- list(x = x, qLimits = qLimits, finalSmooth = finalSmooth, intermediate = intermediate,
                      control = control)
  checkedArgs <- checkArgs(grecArgs = checkedArgs, type = as.character(match.call())[1])

  output <- if(isTRUE(checkedArgs$intermediate)) list() else x
  for(i in seq(dim(x)[3])){
    # Apply the basic function
    allOuts <- with(checkedArgs, detectFronts_internal(x = x[,,i], qLimits = qLimits, finalSmooth = finalSmooth,
                                                       intermediate = intermediate, control = control))

    # Depending on 'intermediate', the output will be a single array or a list of them
    if(isTRUE(checkedArgs$intermediate)){
      output[[i]] <- allOuts
    }else{
      output[,,i] <- allOuts
    }
  }

  return(output)
}
