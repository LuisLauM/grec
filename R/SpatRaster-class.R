#' @rdname getGradients
#' @method getGradients SpatRaster
#' @export
getGradients.SpatRaster <- function(x, method = "BelkinOReilly2009",
                                    intermediate = FALSE, ...){

  checkArgs_df_SpatRaster(x = x)

  out <- apply(
    X = as.array(x = x),
    MARGIN = 3,
    FUN = getGradients,
    method = method,
    intermediate = FALSE,
    checkPrevs = FALSE,
    simplify = FALSE
  ) |>

    lapply(FUN = t) |> lapply(FUN = as.numeric)

  values(x) <- do.call(args = out, what = c)
  varnames(x) <- sprintf("gradient of %s", varnames(x))

  x
}
