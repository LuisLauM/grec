#' @rdname getGradients
#' @method getGradients matrix
#' @export
getGradients.matrix <- function(x, method = "BelkinOReilly2009",
                                intermediate = FALSE, ...){

  # Check arguments
  checkArgs_df_matrix(x = x)

  # Apply method
  output <- getGradients.default(x = x, method = method,
                                 intermediate = intermediate, ...)

  dimnames(output) <- dimnames(x)

  output
}
