#' @rdname getGradients
#' @method getGradients array
#' @export
getGradients.array <- function(x, method = "BelkinOReilly2009",
                               intermediate = FALSE, ...){

  # Check arguments
  checkArgs_df_array(x = x)

  output <- lapply(X = seq(dim(x)[3]),
                   FUN = function(i, ...) getGradients(x = x[,,i], ...),
                   method = method, intermediate = intermediate,
                   checkPrevs = FALSE, ...)

  if(isTRUE(intermediate)){
    index <- names(output[[1]])
    output <- lapply(X = seq_along(index),
                     FUN = function(x) abind(lapply(output, "[[", x), along = 3))

    names(output) <- index
  }else{
    output <- abind(output, along = 3)

    dimnames(output) <- dimnames(x)
  }

  output
}
