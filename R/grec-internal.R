checkArgs_df_matrix <- function(x){
  # Check if x is a valid numerical matrix
  if(!is.numeric(x)){
    cli_abort(
      message = c(
        "{.code x} must be a numeric matrix with environmental data.",
        "i" = "See {.run help(getGradients)}."
      )
    )
  }

  # Check if x is a valid numerical matrix
  if(sum(is.na(as.numeric(x))) == prod(dim(x))){
    cli_abort(
      message = c(
        "There is not any valid values for {.code x}."
      )
    )
  }

  invisible()
}

checkArgs_df_array <- function(x){
  # Check if x is a valid numerical matrix
  if(!is.numeric(x)){
    if(!is.numeric(x)){
      cli_abort(
        message = c(
          "{.code x} must be a numeric array with environmental data.",
          "i" = "See {.run help(getGradients)}."
        )
      )
    }
  }

  # Check if x is a valid numerical matrix
  if(sum(is.na(as.numeric(x))) == prod(dim(x))){
    cli_abort(
      message = c(
        "There is not any valid values for {.code x}."
      )
    )
  }

  invisible()
}

checkArgs_df_list <- function(x){
  # Check if x is a list with 'x', 'y', 'z' dimensions, where z is a numeric matrix/array
  index <- (length(x) == 3 && all(is.element(c("x", "y", "z"), names(x))) && is.numeric(x$x) && is.numeric(x$y) &&
              (is.matrix(x$z) || is.array(x$z)) && is.numeric(x$z))
  if(!index){
    cli_abort(
      message = c(
        "{.code x} must be a XYZ list containing environmental map info (whether a matrix or an array).",
        "i" = "See {.run help(getGradients)}."
      )
    )
  }

  invisible()
}

checkArgs_df_RasterLayer <- function(x){

  if(nlayers(x) < 1){
    cli_abort(
      message = c(
        "RasterLayer object must have at least 1 layer."
      )
    )
  }
  invisible()
}

checkArgs_df_SpatRaster <- function(x){

  if(nlyr(x) < 1){
    cli_abort(
      message = c(
        "SpatRaster object must have at least 1 layer."
      )
    )
  }

  invisible()
}

simplifyChars <- function(x) tolower(gsub(x = x, pattern = "[[:punct:]]", replacement = ""))

checkArgs_prevs <- function(allArgs, type){

  # Define parameters
  method <- simplifyChars(allArgs$method)
  intermediate <- allArgs$intermediate
  ConvolNormalization <- allArgs$ConvolNormalization

  # Check name of method
  methodList <- c("belkinoreilly2009", "medianfilter", "agenbag20031", "agenbag20032")
  if(!is.element(method, methodList)){
    cli_abort(
      message = c(
        "No valid method for gradient calculation."
      )
    )
  }

  # Check 'intermediate'
  if(length(intermediate) != 1 || !is.logical(intermediate)){
    cli_abort(
      message = c(
        "{.code intermediate} must be a single logical value."
      )
    )
  }

  # Check 'ConvolNormalization'
  if(length(ConvolNormalization) != 1 || !is.logical(ConvolNormalization)){
    cli_abort(
      message = c(
        "{.code ConvolNormalization} must be a single logical value."
      )
    )
  }

  invisible()
}
