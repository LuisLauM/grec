#' @rdname detectFronts
#' @method detectFronts list
#' @export
detectFronts.list <- function(x, method = "BelkinOReilly2009", intermediate = FALSE, ...){

  # Check arguments
  checkArgs_df_list(x = x)

  # Apply method
  output <- x
  output$z <- detectFronts(x = x$z, method = method, intermediate = intermediate,
                           checkPrevs = FALSE, ...)

  return(output)
}
