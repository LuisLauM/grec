#' @rdname detectFronts
#' @method detectFronts array
#' @export
detectFronts.array <- function(x, method = "BelkinOReilly2009",
                               intermediate = FALSE, ...){

  # Check arguments
  checkArgs_df_array(x = x)

  output <- lapply(seq(dim(x)[3]), function(i, ...) detectFronts(x = x[,,i], ...),
                   method = method, intermediate = intermediate, checkPrevs = FALSE, ...)

  if(isTRUE(intermediate)){
    index <- names(output[[1]])
    output <- lapply(seq_along(index), function(x) abind(lapply(output, "[[", x), along = 3))

    names(output) <- index
  }else{
    output <- abind(output, along = 3)
  }

  output
}
