#' A small helper function for conditionally plotting points
#'
#' For internal use inside `ggpsd()`; if points argument set to TRUE (the default)
#' points will be added to the log-linear plot (in addition to the log-linear
#' line inerpolation)
#'
#' @param pts Logical value
#'
#' @return
#'

geom_psdpts <- function(pts = points){
  if(pts == TRUE){
    list(
      ggplot2::geom_point()
    )
  }
}
