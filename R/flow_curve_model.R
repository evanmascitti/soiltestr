#' Fit a semi-logarithmic curve to LL data
#'
#' Derives the coefficients for computing the flow curve.
#'
#' @param df a data frame containing water contents and blow counts
#' @param df a data frame containing water contents and blow counts
#'
#' @return The flow curve model object (class "lm")
#' @export
#'
#'
flow_curve_model <- function(df) {
  stats::na.omit(stats::lm(data = df, formula = water_content ~ log(blow_count)) )
}
