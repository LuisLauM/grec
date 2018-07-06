checkArgs_df_matrix <- function(x){
  # Error messages
  msg1 <- "'x' must be a numeric matrix with environmental data. See help(detectFronts)."
  msg2 <- "There is not any valid values for 'x'"

  # Check if x is a valid numerical matrix
  if(!is.numeric(x)){
    stop(msg1)
  }

  # Check if x is a valid numerical matrix
  if(sum(is.na(as.numeric(x))) == prod(dim(x))){
    stop(msg2)
  }

  return(invisible())
}

checkArgs_df_array <- function(x){
  # Error messages
  msg1 <- "'x' must be a numeric array with environmental data. See help(detectFronts)."
  msg2 <- "There is not any valid values for 'x'"

  # Check if x is a valid numerical matrix
  if(!is.numeric(x)){
    stop(msg1)
  }

  # Check if x is a valid numerical matrix
  if(sum(is.na(as.numeric(x))) == prod(dim(x))){
    stop(msg2)
  }

  return(invisible())
}

checkArgs_df_list <- function(x){
  # Error messages
  msg1 <- "'x' must be a XYZ list containing environmental map info (wheter a matrix or an array). See help(detectFronts)."

  # Check if x is a list with 'x', 'y', 'z' dimensions, where z is a numeric matrix/array
  index <- (length(x) == 3 && all(is.element(c("x", "y", "z"), names(x))) && is.numeric(x$x) && is.numeric(x$y) &&
              (is.matrix(x$z) || is.array(x$z)) && is.numeric(x$z))
  if(!index){
    stop(msg1)
  }

  switch(class(x$z),
         matrix = checkArgs_df_matrix(x = x$z),
         array = checkArgs_df_array(x = x$z),
         "Invalid value for 'type'.")

  return(invisible())
}

checkArgs_df_RasterLayer <- function(x){
  return(invisible())
}

checkArgs <- function(allArgs, type){

  # Define parameters
  x             <- allArgs$x
  method        <- allArgs$method
  intermediate  <- allArgs$intermediate

  # Check name of method
  methodList <- c("BelkinOReilly2009")
  if(!is.element(method, methodList)){
    stop("No valid method for gradient calculation.")
  }

  # Check 'intermediate'
  if(length(intermediate) != 1 || !is.logical(intermediate)){
    stop("'intermediate' must be a single logical value.")
  }

  # Check data values by class
  switch(type,
         matrix = checkArgs_df_matrix(x),
         array = checkArgs_df_array(x),
         list = checkArgs_df_list(x),
         RasterLayer = checkArgs_df_RasterLayer(x),
         paste0("There is not any method for ", class(x), "."))

  return(invisible())
}

#' @rdname detectFronts
#' @export
detectFronts.default <- function(x, method = "BelkinOReilly2009", intermediate = FALSE, ...){

  output <- switch(method,
                   BelkinOReilly2009 = detectFronts_BelkinOReilly2009(x = x,
                                                                      intermediate = intermediate,
                                                                      ...))

  return(output)
}

# detectFronts_LauMedrano <- function(x, qLimits, finalSmooth, intermediate, control){
#
#   if(is.null(qLimits)){
#     qLimits <- c(0.9, 0.99)
#   }
#
#   # Create empty list for outputs
#   if(intermediate){
#     output <- array(data = NA, dim = c(dim(x),  ifelse(isTRUE(finalSmooth), 6, 5)))
#     output[,,1] <- x
#   }
#
#   # Make a first smooth
#   preMatrix <- medianFilter(X = x,
#                             radius = control$firstSmooth$radius,
#                             times = control$firstSmooth$times)
#
#   if(intermediate){
#     output[,,2] <- preMatrix
#   }
#
#   # Define sobel kernel values
#   sobelKernel <- control$sobelStrength*control$kernelValues
#
#   # Define sobel kernels
#   sobelH <- matrix(data = sobelKernel, nrow = 3, byrow = TRUE)
#   sobelV <- matrix(data = sobelKernel, nrow = 3, byrow = FALSE)
#
#   # Apply sobel filters (horizontal and vertical)
#   filteredH <- convolution2D(X = preMatrix, kernel = sobelH, noNA = TRUE)
#   filteredV <- convolution2D(X = preMatrix, kernel = sobelV, noNA = TRUE)
#
#   if(intermediate){
#     output[,,3] <- filteredH
#     output[,,4] <- filteredV
#   }
#
#   # Calculate gradient
#   newSobel <- sqrt(filteredH^2 + filteredV^2)
#   qLimits <- quantile(x = as.numeric(newSobel), probs = qLimits, na.rm = TRUE)
#   newSobel[newSobel < qLimits[1] | newSobel > qLimits[2]] <- NA
#
#   if(intermediate){
#     output[,,5] <- newSobel
#   }
#
#   # Clear noisy signals
#   if(isTRUE(finalSmooth)){
#     clearNoise <- medianFilter(X = newSobel,
#                                radius = control$clearNoise$radius,
#                                times = control$clearNoise$times)
#
#     if(intermediate){
#       output[,,6] <- clearNoise
#     }else{
#       output <- clearNoise
#     }
#   }else if(!intermediate){
#     output <- newSobel
#   }
#
#   return(output)
# }

detectFronts_BelkinOReilly2009 <- function(x, finalSmooth, intermediate, ...){
  # Create empty list for outputs
  if(intermediate){
    output <- array(data = NA, dim = c(dim(x), 5))
    output[,,1] <- x
  }

  control_default <- list(inner_radius = 3,
                          outer_radius = 5,
                          times = 1,
                          kernelValues = c(-1, -2, -1, 0, 0, 0, 1, 2, 1))
  extraParams <- modifyList(x = control_default, val = list(...))

  # Apply a smooth (Contextual Median Filter)
  preMatrix <- contextualMF(X = x, inner_radius = extraParams$inner_radius,
                            outer_radius = extraParams$outer_radius, times = extraParams$times)

  if(intermediate){
    output[,,2] <- preMatrix
  }

  # Define sobel kernel values
  sobelKernel <- extraParams$kernelValues

  # Define sobel kernels
  sobelH <- matrix(data = sobelKernel, nrow = 3, byrow = TRUE)
  sobelV <- matrix(data = sobelKernel, nrow = 3, byrow = FALSE)

  # Apply sobel filters (horizontal and verticaly)
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

  # Return output
  if(intermediate){
    return(output)
  }else{
    dimnames(newSobel) <- dimnames(x)

    return(newSobel)
  }
}
