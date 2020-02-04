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
  msg1 <- "'x' must be a XYZ list containing environmental map info (whether a matrix or an array). See help(detectFronts)."

  # Check if x is a list with 'x', 'y', 'z' dimensions, where z is a numeric matrix/array
  index <- (length(x) == 3 && all(is.element(c("x", "y", "z"), names(x))) && is.numeric(x$x) && is.numeric(x$y) &&
              (is.matrix(x$z) || is.array(x$z)) && is.numeric(x$z))
  if(!index){
    stop(msg1)
  }

  return(invisible())
}

checkArgs_df_RasterLayer <- function(x){
  return(invisible())
}

checkArgs_prevs <- function(allArgs, type){

  # Define parameters
  method        <- allArgs$method
  intermediate  <- allArgs$intermediate

  # Check name of method
  methodList <- c("BelkinOReilly2009", "median_filter")
  if(!is.element(method, methodList)){
    stop("No valid method for gradient calculation.")
  }

  # Check 'intermediate'
  if(length(intermediate) != 1 || !is.logical(intermediate)){
    stop("'intermediate' must be a single logical value.")
  }

  return(invisible())
}
