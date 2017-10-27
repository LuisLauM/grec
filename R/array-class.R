#' @rdname detectFronts
#' @method detectFronts array
#' @export
detectFronts.array <- function(x, method = "BelkinOReilly2009", finalSmooth = FALSE, intermediate = FALSE,
                               control = list(), ...){

  # Check and validation of arguments
  checkedArgs <- list(x = x, method = method, finalSmooth = finalSmooth, intermediate = intermediate,
                      control = control, ...)
  checkedArgs <- checkArgs(grecArgs = checkedArgs, type = as.character(match.call())[1])

  output <- if(isTRUE(checkedArgs$intermediate)) list() else x
  for(i in seq(dim(x)[3])){
    # Apply the basic function
    allOuts <- with(checkedArgs,
                    switch(method,
                           BelkinOReilly2009 = detectFronts_BelkinOReilly2009(x = x[,,i],
                                                                      finalSmooth = finalSmooth,
                                                                      intermediate = intermediate,
                                                                      control = control),
                           LauMedrano = detectFronts_LauMedrano(x = x[,,i],
                                                                qLimits = list(...)$qLimits,
                                                                finalSmooth = finalSmooth,
                                                                intermediate = intermediate,
                                                                control = control)))

    # Depending on 'intermediate', the output will be a single array or a list of them
    if(isTRUE(checkedArgs$intermediate)){
      output[[i]] <- allOuts
    }else{
      output[,,i] <- allOuts
    }
  }

  return(output)
}
