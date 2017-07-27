checkArgs <- function(grecArgs, type){

  output <- switch(type,
                   detectFronts = checkArgs_detectFronts(grecArgs),
                   extraParams = checkArgs_extraParams(grecArgs),
                   "Invalid value for 'type'.")

  return(output)
}


checkArgs_detectFronts <- function(allArgs){

  x <- allArgs$x
  qLimits <- sort(unique(allArgs$qLimits))
  finalSmooth <- allArgs$finalSmooth
  intermediate <- allArgs$intermediate
  control <- allArgs$control

  msg1 <- "If 'x' is a list, it must contain info for environmental map. See help(detectFronts)."
  msg2 <- "If 'x' is a matrix, it must be a numerical matrix with environmental data. See help(detectFronts)."
  msg3 <- "'x' must a matrix or list containing numerical values of environmental map. See help(detectFronts)."

  if(is.list(x)){

    # Check if x is a list with 'x', 'y', 'z' dimensions, where z is a numeric matrix
    index <- (length(x) == 3 && all(is.element(c("x", "y", "z"), letters)) &&
                is.matrix(x$z) && is.numeric(x$z))
    if(!index){
      stop(msg1)
    }
  }else if(is.matrix(x)){

    # Check if x is a valid numerical matrix
    index <- is.matrix(x$z) && is.numeric(x$z)
    if(!index){
      stop(msg2)
    }
  }else{
    stop(msg3)
  }

  msg4 <- "Invalid value for 'intermediate', it will take its default value (TRUE)."
  if(!is.logical(intermediate)){
    intermediate <- TRUE
    warning(msg4)
  }

  # Check if qLimits is a numeric vector of length 1 or 2
  msg5 <- "'qLimits' must be a numeric vector with values between 0 and 1."
  if(!is.numeric(qLimits)){
    stop(msg5)
  }

  msg6 <- "'qLimits' must be a numeric vector of length 1 or 2 and values between 0 and 1. See help(detectFronts)."
  if(any(qLimits < 0 | qLimits > 1)){
    stop(msg6)
  }

  if(length(qLimits) == 1){
    if(isTRUE(all.equal(qLimits, 1))){
      allArgs$qLimits <- c(qLimits, qLimits)
    }else{
      allArgs$qLimits <- c(qLimits, qLimits + (1 - qLimits)/2)
    }
  }else if(length(qLimits) == 2){
    allArgs$qLimits <- sort(qLimits)
  }else{
    stop(msg6)
  }

  msg7 <- "'control' must be a named list with arguments for imagine functions. See help(detectFronts)."
  if(!is.list(control)){
    stop(msg7)
  }else{
    # Define extra parameters for filter
    control_default <- extraParams(fx = "detectFronts")

    # Merge two list of control params
    allArgs$control <- modifyList(control_default$detectFronts, control)
  }

  msg8 <- "Invalid value for 'finalSmooth', it will take its default value (FALSE)."
  if(!is.logical(finalSmooth)){
    intermediate <- FALSE
    warning(msg8)
  }

  return(allArgs)
}

checkArgs_extraParams <- function(allArgs){
  fx <- allArgs$fx

  msg1 <- "'fx' must be a character vector with a valid name of a grec function. See help(extraParams)."
  validFunctions <- c("detectFronts")
  index <- is.vector(fx) && is.character(fx) && all(is.element(fx, validFunctions))
  if(!index){
    stop(msg1)
  }

  return(allArgs)
}

detectFronts_internal <- function(x, qLimits, finalSmooth, intermediate, control){

  # Create empty list for outputs
  if(intermediate){
    output <- list()
    output[[1]] <- list(x = x$x,
                        y = x$y,
                        z = x$z)
  }

  # Make a first smooth
  preMatrix <- medianFilter(dataMatrix = x$z,
                            radius = control$firstSmooth$radius,
                            times = control$firstSmooth$times)

  if(intermediate){
    output[[2]] <- list(x = x$x,
                        y = x$y,
                        z = preMatrix)
  }

  # Define sobel kernel values
  sobelKernel <- control$sobelStrength*control$kernelValues

  # Define sobel kernels
  sobelH <- matrix(data = sobelKernel, nrow = 3, byrow = TRUE)
  sobelV <- matrix(data = sobelKernel, nrow = 3, byrow = FALSE)

  # Apply sobel filters (horizontal and vertical)
  filteredH <- convolution2D(dataMatrix = preMatrix, kernel = sobelH, noNA = TRUE)
  filteredV <- convolution2D(dataMatrix = preMatrix, kernel = sobelV, noNA = TRUE)

  if(intermediate){
    output[[3]] <- list(x = x$x,
                        y = x$y,
                        z = filteredH)
    output[[4]] <- list(x = x$x,
                        y = x$y,
                        z = filteredV)
  }

  # Calculate gradient
  newSobel <- sqrt(filteredH^2 + filteredV^2)
  qLimits <- quantile(x = as.numeric(newSobel), probs = qLimits, na.rm = TRUE)
  newSobel[newSobel < qLimits[1] | newSobel > qLimits[2]] <- NA

  if(intermediate){
    output[[5]] <- list(x = x$x,
                        y = x$y,
                        z = newSobel)
  }

  # Clear noisy signals
  if(isTRUE(finalSmooth)){
    newSobel <- medianFilter(dataMatrix = newSobel,
                             radius = control$clearNoise$radius,
                             times = control$clearNoise$times)

    output[[6]] <- list(x = x$x,
                        y = x$y,
                        z = newSobel)
  }

  if(intermediate){
    names(output) <- c("original", "first_smooth", "sobel_H", "sobel_V", "gradient",
                       if(isTRUE(finalSmooth)) "noise_cleared" else NULL)
  }else{
    output <- list(x = x$x,
                   y = x$y,
                   z = newSobel)
  }

  return(output)
}

extraParams_internal <- function(fx){
  output <- list()

  for(i in seq_along(fx)){
    output[[i]] <- switch(fx[i],
                          detectFronts = list(firstSmooth = list(radius = 5,
                                                                times = 10),
                                             kernelValues = c(-1, -2, -1, 0, 0, 0, 1, 2, 1),
                                             sobelStrength = 10,
                                             clearNoise = list(radius = 5,
                                                               times = 1)),
                          paste0("There is no extra parameters for ", fx[i], "."))
  }

  names(output) <- fx

  return(output)
}
