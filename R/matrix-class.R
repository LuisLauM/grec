#' @rdname detectFronts
#' @method detectFronts matrix
#' @export
detectFronts.matrix <- function(x, method = "BelkinOReilly2009", intermediate = FALSE, ...){

  # Check arguments
  checkArgs_df_matrix(x = x)

  # Apply method
  output <- detectFronts.default(x = x, method = method, intermediate = intermediate, ...)

  return(output)
}
