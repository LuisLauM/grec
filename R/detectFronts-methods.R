
#' @rdname detectFronts
#' @export
detectFronts.default <- function(x, method = "BelkinOReilly2009", intermediate = FALSE, ...){

  # Decide the method
  output <- switch(method,
                   BelkinOReilly2009 = detectFronts_BelkinOReilly2009(x = x,
                                                                      intermediate = intermediate,
                                                                      ...),
                   median_filter = detectFronts_MF(x = x, intermediate = intermediate, ...))

  return(output)
}

detectFronts_BelkinOReilly2009 <- function(x, intermediate, ...){
  # Create empty list for outputs
  if(intermediate){
    output <- array(data = NA, dim = c(dim(x), 5))
    output[,,1] <- x
  }

  control_default <- list(times = 1,
                          kernelValues = c(-1, -2, -1, 0, 0, 0, 1, 2, 1))
  extraParams <- modifyList(x = control_default, val = list(...))

  # Apply a smooth (Contextual Median Filter)
  preMatrix <- contextualMF(X = x, times = extraParams$times)

  if(intermediate){
    output[,,2] <- preMatrix
  }

  # Define sobel kernel values
  sobelKernel <- extraParams$kernelValues

  # Define sobel kernels
  sobelH <- matrix(data = sobelKernel, nrow = 3, byrow = TRUE)
  sobelV <- matrix(data = sobelKernel, nrow = 3, byrow = FALSE)

  # Apply sobel filters (horizontal and verticaly)
  filteredH <- convolution2D(X = preMatrix, kernel = sobelH)
  filteredV <- convolution2D(X = preMatrix, kernel = sobelV)

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
    dimnames(output) <- c(dimnames(x),
                          c("original", "ContextualMedianFilter", "sobel_out_byRows", "sobel_out_byCols", "final"))

    return(output)
  }else{
    dimnames(newSobel) <- dimnames(x)

    return(newSobel)
  }
}

detectFronts_MF <- function(x, intermediate, ...){
  # Create empty list for outputs
  if(intermediate){
    output <- array(data = NA, dim = c(dim(x), 5))
    output[,,1] <- x
  }

  control_default <- list(radius = 3,
                          times = 1,
                          kernelValues = c(-1, -2, -1, 0, 0, 0, 1, 2, 1))
  extraParams <- modifyList(x = control_default, val = list(...))

  # Apply a smooth (Median Filter)
  preMatrix <- medianFilter(X = x, radius = extraParams$radius, times = extraParams$times)

  if(intermediate){
    output[,,2] <- preMatrix
  }

  # Define sobel kernel values
  sobelKernel <- extraParams$kernelValues

  # Define sobel kernels
  sobelH <- matrix(data = sobelKernel, nrow = 3, byrow = TRUE)
  sobelV <- matrix(data = sobelKernel, nrow = 3, byrow = FALSE)

  # Apply sobel filters (horizontal and verticaly)
  filteredH <- convolution2D(X = preMatrix, kernel = sobelH)
  filteredV <- convolution2D(X = preMatrix, kernel = sobelV)

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
    dimnames(output) <- c(dimnames(x),
                          c("original", "MedianFilter", "sobel_out_byRows", "sobel_out_byCols", "final"))

    return(output)
  }else{
    dimnames(newSobel) <- dimnames(x)

    return(newSobel)
  }
}
