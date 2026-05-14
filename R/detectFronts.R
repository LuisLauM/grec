#' @title Apply gradient-based methodologies to environmental data
#'
#' @param ... Same arguments than \link[grec]{getGradients}.
#'
#' @description
#' `r lifecycle::badge("deprecated")`
#'
#' This function empowers users to analyze data from various sources,
#' including numeric `matrix`, `array`s, XYZ-`list`s,
#' `SpatRaster`s, or `RasterLayer`s*, by applying gradient-seeking
#' methodologies.
#'
#' @details
#' Since version 1.6.0, this function has been entirely replaced by
#' \link[grec]{getGradients}. As of version 2.0.0, `detectFronts` will no longer
#' be available.
#'
#' @export
detectFronts <- function(...){

  deprecate_soft(when = "1.6.0", what = "detectFronts()", with = "getGradients()")

  getGradients(...)
}
