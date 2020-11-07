#' Calculate the liquid limit from the flow curve
#'
#' Uses a blow count of 25 to interpolate the liquid limit.
#'
#' @param flow_curve the model object to use when computing the LL
#'
#' @return numeric vector (length 1) containing the liquid limit as a decimal
#' @export
#'
compute_LL <- function(flow_curve) {
  stats::predict.lm(object = flow_curve, newdata = data.frame(blow_count= c(25)) )
}
