#' @rdname getGradients
#' @method getGradients list
#' @export
getGradients.list <- function(x, method = "BelkinOReilly2009",
                              intermediate = FALSE, ...){

  # Check arguments
  checkArgs_df_list(x = x)

  # Apply method
  x$z <- getGradients(x = x$z, method = method, intermediate = intermediate,
                      checkPrevs = FALSE, ...)

  x
}
