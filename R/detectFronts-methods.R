
#' @rdname detectFronts
#' @export
detectFronts.default <- function(x, method = "BelkinOReilly2009",
                                 intermediate = FALSE,
                                 ConvolNormalization = FALSE, ...){

  # Decide the method
  output <- switch(method,
                   BelkinOReilly2009 = detectFronts_BelkinOReilly2009(a = x,
                                                                      intermediate = intermediate,
                                                                      ConvNorm = ConvolNormalization,
                                                                      ...),
                   median_filter = detectFronts_MF(x = x,
                                                   intermediate = intermediate,
                                                   ConvNorm = ConvolNormalization,
                                                   ...))

  return(output)
}

detectFronts_BelkinOReilly2009 <- function(a, intermediate, ConvNorm, ...){

  control_default <- list(kernelValues = c(-1, -2, -1, 0, 0, 0, 1, 2, 1))
  extraParams <- modifyList(x = control_default, val = list(...))

  # Apply a smooth (Contextual Median Filter)
  A <- contextualMF(X = a)

  # Define sobel kernel values
  sobelKernel <- extraParams$kernelValues

  # Define sobel kernels
  GX <- matrix(data = sobelKernel, nrow = 3)
  GY <- apply(t(GX), 2, rev) # GX rotated 90 deg counter-clockwise

  # Apply sobel filters (horizontal and verticaly)
  Gx <- convolution2D(X = A, kernel = GX)
  Gy <- convolution2D(X = A, kernel = GY)

  # Apply IDL normalization
  if(ConvNorm){
    Gx <- Gx/sum(abs(sobelKernel), na.rm = TRUE)
    Gy <- Gy/sum(abs(sobelKernel), na.rm = TRUE)
  }

  # Calculate gradient direction
  if(intermediate){
    GD <- atan(Gy/Gx)
  }

  # Calculate gradient magnitude
  GM <- sqrt(Gx^2 + Gy^2)

  # Return output
  if(intermediate){

    output <- list(original = a,
                   CMF = A,
                   Gx = Gx,
                   Gy = Gy,
                   GD = GD,
                   GM = GM)
  }else{
    output <- GM
  }

  output
}

detectFronts_MF <- function(x, intermediate, ConvNorm, ...){

  control_default <- list(radius = 3,
                          times = 1,
                          kernelValues = c(-1, -2, -1, 0, 0, 0, 1, 2, 1))
  extraParams <- modifyList(x = control_default, val = list(...))

  # Apply a smooth (Median Filter)
  preMatrix <- medianFilter(X = x, radius = extraParams$radius, times = extraParams$times)

  # Define sobel kernel values
  sobelKernel <- extraParams$kernelValues

  # Define sobel kernels
  sobelV <- matrix(data = sobelKernel, nrow = 3)
  sobelH <- apply(t(sobelV), 2, rev) # sobelV rotated 90 deg counter-clockwise

  # Apply sobel filters (horizontal and verticaly)
  filteredV <- convolution2D(X = preMatrix, kernel = sobelV)
  filteredH <- convolution2D(X = preMatrix, kernel = sobelH)

  # Apply IDL normalization
  if(ConvNorm){
    filteredV <- filteredV/sum(abs(sobelKernel), na.rm = TRUE)
    filteredH <- filteredH/sum(abs(sobelKernel), na.rm = TRUE)
  }

  # Calculate gradient
  newSobel <- sqrt(filteredV^2 + filteredH^2)

  # Return output
  if(intermediate){

    output <- list(original = x,
                   median_filter = preMatrix,
                   sobelV = Gx,
                   sobelH = Gy,
                   gradients = newSobel)
  }else{
    output <- newSobel
  }

  output
}
