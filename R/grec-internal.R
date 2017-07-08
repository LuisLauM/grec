checkArgs <- function(grecArgs, type){

  output <- switch(type,
                   frontDetect = checkArgs_frontDetect(grecArgs),
                   "Invalid value for 'type'.")

  return(output)
}


checkArgs_frontDetect <- function(allArgs){

  envirData <- allArgs$envirData
  thresholds <- allArgs$thresholds
  stepByStep <- allArgs$stepByStep

  msg1 <- "If 'envirData' is a list, it must contain info for environmental map. See help(frontDetect)."
  msg2 <- "If 'envirData' is a matrix, it must be a numerical matrix with environmental data. See help(frontDetect)."
  msg3 <- "'envirData' must a matrix or list containing numerical values of environmental map. See help(frontDetect)."

  if(is.list(envirData)){

    # Check if envirData is a list with 'x', 'y', 'z' dimensions, where z is a numeric matrix
    index <- (length(envirData) == 3 && all(is.element(c("x", "y", "z"), letters)) &&
                is.matrix(envirData$z) && is.numeric(envirData$z))
    if(!index){
      stop(msg1)
    }
  }else if(is.matrix(envirData)){

    # Check if envirData is a valid numerical matrix
    index <- is.matrix(envirData$z) && is.numeric(envirData$z)
    if(!index){
      stop(msg2)
    }
  }else{
    stop(msg3)
  }

  msg4 <- "Invalid value for 'stepByStep', it will take its default value (TRUE)."
  if(!is.logical(stepByStep)){
    stepByStep <- TRUE
    warning(msg4)
  }

  # Check if thresholds is a numeric vector of length 1 or 2
  msg5 <- "'thresholds' must be a numeric vector."
  if(!is.numeric(thresholds)){
    stop(msg5)
  }

  msg6 <- "'thresholds' must be a numeric vector of length 1 or 2. See help(frontDetect)."
  if(length(thresholds) == 1){
    allArgs$thresholds <- c(thresholds, 5*thresholds)
  }else if(length(thresholds) == 2){
    allArgs$thresholds <- sort(thresholds)
  }else{
    stop(msg6)
  }

  return(allArgs)
}

frontDetect_internal <- function(envirData, thresholds, stepByStep, extraParams){

  # Create empty list for outputs
  if(stepByStep){
    output <- list()
    output[[1]] <- list(x = envirData$x,
                        y = envirData$y,
                        z = envirData$z)
  }

  # Make a first smooth
  preMatrix <- medianFilter(dataMatrix = envirData$z,
                            radius = extraParams$firstSmooth$radius,
                            times = extraParams$firstSmooth$times)

  if(stepByStep){
    output[[2]] <- list(x = envirData$x,
                        y = envirData$y,
                        z = preMatrix)
  }

  # Define sobel kernel values
  sobelKernel <- extraParams$sobelStrength*c(-1, -2, -1, 0, 0, 0, 1, 2, 1)

  # Define sobel kernels
  sobelH <- matrix(data = sobelKernel, nrow = 3, byrow = TRUE)
  sobelV <- matrix(data = sobelKernel, nrow = 3, byrow = FALSE)

  # Apply sobel filters (horizontal and vertical)
  filteredH <- convolution2D(dataMatrix = preMatrix, kernel = sobelH, noNA = TRUE)
  filteredV <- convolution2D(dataMatrix = preMatrix, kernel = sobelV, noNA = TRUE)

  if(stepByStep){
    output[[3]] <- list(x = envirData$x,
                        y = envirData$y,
                        z = filteredH)
    output[[4]] <- list(x = envirData$x,
                        y = envirData$y,
                        z = filteredV)
  }

  # Calculate gradient
  newSobel <- sqrt(filteredH^2 + filteredV^2)
  newSobel[newSobel < thresholds[1] | newSobel > thresholds[2]] <- NA

  if(stepByStep){
    output[[5]] <- list(x = envirData$x,
                        y = envirData$y,
                        z = newSobel)
  }

  # Clear noisy signals
  newSobel <- medianFilter(dataMatrix = newSobel,
                           radius = extraParams$clearNoise$radius,
                           times = extraParams$clearNoise$times)

  if(stepByStep){
    output[[6]] <- list(x = envirData$x,
                        y = envirData$y,
                        z = newSobel)
    names(output) <- c("original", "first_smooth", "sobel_H", "sobel_V", "gradient", "noise_cleared")
  }else{
    output <- list(x = envirData$x,
                   y = envirData$y,
                   z = newSobel)
  }

  return(output)
}
