#' @rdname detectFronts
#' @method detectFronts array
#' @export
detectFronts.array <- function(x, qLimits = c(0.9, 0.99), finalSmooth = FALSE, intermediate = FALSE,
                                control = list()){

  output <- if(isTRUE(intermediate)) list() else x
  for(i in seq(dim(x)[3])){
    # Apply the basic function
    allOuts <- detectFronts.default(x = x[,,i], qLimits = qLimits, finalSmooth = finalSmooth,
                                    intermediate = intermediate, control = control)

    # Depending on 'intermediate', the output will be a single array or a list of them
    if(isTRUE(intermediate)){
      output[[i]] <- allOuts
    }else{
      output[,,i] <- allOuts$z
    }
  }

  if(isTRUE(intermediate)){
    names(output) <- seq(dim(x)[3])
  }

  return(output)
}
