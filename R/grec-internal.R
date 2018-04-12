checkArgs <- function(grecArgs, type){

  output <- switch(type,
                   detectFronts.default = checkArgs_detectFronts(grecArgs),
                   detectFronts.array = checkArgs_detectFronts(grecArgs),
                   detectFronts.RasterLayer = checkArgs_detectFronts(grecArgs),
                   extraParams = checkArgs_extraParams(grecArgs),
                   "Invalid value for 'type'.")

  return(output)
}


checkArgs_detectFronts <- function(allArgs, ...){

  # Define parameters
  x             <- allArgs$x
  method        <- allArgs$method
  finalSmooth   <- allArgs$finalSmooth
  intermediate  <- allArgs$intermediate
  control       <- allArgs$control

  methodList <- c("LauMedrano", "BelkinOReilly2009")
  if(!is.element(method, methodList)){
    stop("No valid method for front calculation.")
  }

  # Define stop messages
  msg1 <- "'X' must be a XYZ list containing environmental map info (wheter a matrix or an array). See help(detectFronts)."
  msg2 <- "'X' must be a numeric matrix with environmental data. See help(detectFronts)."
  msg3 <- "'X' must be a numeric array  with environmental data. See help(detectFronts)."
  msg4 <- "'X' must a matrix, list, RasterLayer or array with environmental data See help(detectFronts)."

  if(is.list(x)){
    # Check if x is a list with 'x', 'y', 'z' dimensions, where z is a numeric matrix
    index <- (length(x) == 3 && all(is.element(c("x", "y", "z"), letters)) &&
                (is.matrix(x$z) || is.array(x$z)) && is.numeric(x$z))
    if(!index){
      stop(msg1)
    }
  }else if(is.matrix(x)){
    # Check if x is a valid numerical matrix
    index <- is.matrix(x) && is.numeric(x)
    if(!index){
      stop(msg2)
    }

    allArgs$x <- x
  }else if(is.array(x)){
    # Check if x is a valid numerical array
    if(!is.numeric(x) || length(dim(x)) > 3){
      stop(msg3)
    }
  }else if(!class(x) != "RasterLayer"){
    stop(msg4)
  }

  msg1 <- "Invalid value for 'intermediate', it will take its default value (TRUE)."
  if(!is.logical(intermediate)){
    intermediate <- TRUE
    warning(msg1)
  }

  if(method == "LauMedrano"){

    allArgs$qLimits <- list(...)$qLimits
    if(is.null(allArgs$qLimits)){
      allArgs$qLimits <- c(0.9, 0.99)
    }

    qLimits <- sort(unique(allArgs$qLimits))

    msg1 <- "'qLimits' must be a numeric vector of length 1 or 2 and values between 0 and 1. See help(detectFronts)."
    if(!is.numeric(qLimits) || any(qLimits < 0 | qLimits > 1)){
      stop(msg1)
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
      stop(msg1)
    }
  }

  msg1 <- "'control' must be a named list with arguments for imagine functions. See help(detectFronts)."
  if(!is.list(control)){
    stop(msg1)
  }else{
    # Define extra parameters for filter
    control_default <- extraParams(fx = "detectFronts")

    # Merge two list of control params
    allArgs$control <- modifyList(x = control_default, val = control)
  }

  msg1 <- "Invalid value for 'finalSmooth', it will take its default value (FALSE)."
  if(!is.logical(finalSmooth)){
    intermediate <- FALSE
    warning(msg1)
  }

  return(allArgs)
}

checkArgs_extraParams <- function(allArgs){
  fx <- allArgs$fx

  msg1 <- "'fx' must be a valid grec function."

  grecFx <- ls(name = "package:grec")
  if(!is.character(fx) || length(fx) != 1 || !is.element(fx, grecFx)){
    stop(msg1)
  }

  return(allArgs)
}

#' @rdname detectFronts
#' @export
detectFronts.default <- function(x, method = "BelkinOReilly2009", finalSmooth = FALSE,
                                 intermediate = FALSE, control = list(), ...){
  # Check and validation of arguments
  checkedArgs <- list(x = x, method = method, finalSmooth = finalSmooth, intermediate = intermediate,
                      control = control, ...)
  checkedArgs <- checkArgs(grecArgs = checkedArgs, type = as.character(match.call())[1])

  # Get fronts
  if(is.list(checkedArgs$x)){
    # If is a list (with array or matrix info), it preserves the x and y dimension
    if(is.matrix(checkedArgs$x$z)){
      outMatrix <- with(checkedArgs,
                        switch(method,
                               BelkinOReilly2009 = detectFronts_BelkinOReilly2009(x = x$z,
                                                                                  finalSmooth = finalSmooth,
                                                                                  intermediate = intermediate,
                                                                                  control = control),
                               LauMedrano = detectFronts_LauMedrano(x = x$z,
                                                                    qLimits = list(...)$qLimits,
                                                                    finalSmooth = finalSmooth,
                                                                    intermediate = intermediate,
                                                                    control = control)))

      if(isTRUE(checkedArgs$intermediate)){
        output <- list()
        for(i in seq(dim(outMatrix)[3])){
          output[[i]] <- list(x = checkedArgs$x$x,
                              y = checkedArgs$x$y,
                              z = outMatrix[,,i])
        }

        names(output) <- c("original", "first_smooth", "sobel_H", "sobel_V", "gradient",
                           if(isTRUE(checkedArgs$finalSmooth)) "noise_cleared" else NULL)
      }else{
        output <- list(x = checkedArgs$x$x,
                       y = checkedArgs$x$y,
                       z = outMatrix)
      }
    }else if(is.array(checkedArgs$x$z)){
      outMatrix <- with(checkedArgs,
                        detectFronts.array(x = x$z, method = method, finalSmooth = finalSmooth,
                                           intermediate = intermediate, control = control))

      if(isTRUE(checkedArgs$intermediate)){
        output <- list()

        for(i in seq(dim(outMatrix[[1]])[3])){
          output[[i]] <- list(x = checkedArgs$x$x,
                              y = checkedArgs$x$y,
                              z = sapply(outMatrix, function(x, index) x[,,index], index = i))
        }

        names(output) <- c("original", "first_smooth", "sobel_H", "sobel_V", "gradient",
                           if(isTRUE(checkedArgs$finalSmooth)) "noise_cleared" else NULL)
      }else{
        output <- list(x = checkedArgs$x$x,
                       y = checkedArgs$x$y,
                       z = outMatrix)
      }
    }
  }else if(is.matrix(checkedArgs$x)){
    output <- with(checkedArgs,
                   switch(method,
                          BelkinOReilly2009 = detectFronts_BelkinOReilly2009(x = x,
                                                                             finalSmooth = finalSmooth,
                                                                             intermediate = intermediate,
                                                                             control = control),
                          LauMedrano = detectFronts_LauMedrano(x = x,
                                                               qLimits = list(...)$qLimits,
                                                               finalSmooth = finalSmooth,
                                                               intermediate = intermediate,
                                                               control = control)))
  }

  return(output)
}

detectFronts_LauMedrano <- function(x, qLimits, finalSmooth, intermediate, control){

  if(is.null(qLimits)){
    qLimits <- c(0.9, 0.99)
  }

  # Create empty list for outputs
  if(intermediate){
    output <- array(data = NA, dim = c(dim(x),  ifelse(isTRUE(finalSmooth), 6, 5)))
    output[,,1] <- x
  }

  # Make a first smooth
  preMatrix <- medianFilter(X = x,
                            radius = control$firstSmooth$radius,
                            times = control$firstSmooth$times)

  if(intermediate){
    output[,,2] <- preMatrix
  }

  # Define sobel kernel values
  sobelKernel <- control$sobelStrength*control$kernelValues

  # Define sobel kernels
  sobelH <- matrix(data = sobelKernel, nrow = 3, byrow = TRUE)
  sobelV <- matrix(data = sobelKernel, nrow = 3, byrow = FALSE)

  # Apply sobel filters (horizontal and vertical)
  filteredH <- convolution2D(X = preMatrix, kernel = sobelH, noNA = TRUE)
  filteredV <- convolution2D(X = preMatrix, kernel = sobelV, noNA = TRUE)

  if(intermediate){
    output[,,3] <- filteredH
    output[,,4] <- filteredV
  }

  # Calculate gradient
  newSobel <- sqrt(filteredH^2 + filteredV^2)
  qLimits <- quantile(x = as.numeric(newSobel), probs = qLimits, na.rm = TRUE)
  newSobel[newSobel < qLimits[1] | newSobel > qLimits[2]] <- NA

  if(intermediate){
    output[,,5] <- newSobel
  }

  # Clear noisy signals
  if(isTRUE(finalSmooth)){
    clearNoise <- medianFilter(X = newSobel,
                               radius = control$clearNoise$radius,
                               times = control$clearNoise$times)

    if(intermediate){
      output[,,6] <- clearNoise
    }else{
      output <- clearNoise
    }
  }else if(!intermediate){
    output <- newSobel
  }

  return(output)
}

detectFronts_BelkinOReilly2009 <- function(x, finalSmooth, intermediate, control){
  # Create empty list for outputs
  if(intermediate){
    output <- array(data = NA, dim = c(dim(x),  ifelse(isTRUE(finalSmooth), 6, 5)))
    output[,,1] <- x
  }

  # Make a first smooth
  preMatrix <- contextualMF(X = x, inner_radius = control$firstSmooth$inner_radius,
                            outer_radius = control$firstSmooth$outer_radius, times = control$firstSmooth$times)

  if(intermediate){
    output[,,2] <- preMatrix
  }

  # Define sobel kernel values
  sobelKernel <- control$sobelStrength*control$kernelValues

  # Define sobel kernels
  sobelH <- matrix(data = sobelKernel, nrow = 3, byrow = TRUE)
  sobelV <- matrix(data = sobelKernel, nrow = 3, byrow = FALSE)

  # Apply sobel filters (horizontal and vertical)
  filteredH <- convolution2D(X = preMatrix, kernel = sobelH, noNA = TRUE)
  filteredV <- convolution2D(X = preMatrix, kernel = sobelV, noNA = TRUE)

  # Normalize values
  normfactor <- sum(abs(sobelKernel), na.rm = TRUE)
  filteredH <- filteredH/normfactor
  filteredV <- filteredV/normfactor

  if(intermediate){
    output[,,3] <- filteredH
    output[,,4] <- filteredV
  }

  # Calculate gradient
  newSobel <- sqrt(filteredH^2 + filteredV^2)

  if(intermediate){
    output[,,5] <- newSobel
  }

  # Clear noisy signals
  if(isTRUE(finalSmooth)){
    clearNoise <- medianFilter(X = newSobel,
                               radius = control$clearNoise$radius,
                               times = control$clearNoise$times)

    if(intermediate){
      output[,,6] <- clearNoise
    }else{
      output <- clearNoise
    }
  }else if(!intermediate){
    output <- newSobel
  }

  return(output)
}

extraParams_internal <- function(fx){
  output <- switch(fx,
                   detectFronts = list(firstSmooth = list(inner_radius = 3,
                                                          outer_radius = 5,
                                                          times = 1),
                                       kernelValues = c(-1, -2, -1, 0, 0, 0, 1, 2, 1),
                                       sobelStrength = 1,
                                       clearNoise = list(radius = 5,
                                                         times = 1)),
                   paste0("There is no extra parameters for ", fx[i], "."))

  return(output)
}
