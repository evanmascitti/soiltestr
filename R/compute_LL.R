#' \lifecycle{maturing}
#'
#' @title Calculate the liquid limit from the flow curve
#'
#' @description Uses a blow count of 25 to interpolate the liquid limit.
#'
#' @param df data frame containing colums named `water_content` and
#'   `blow_count`. Water contents may be easily computed with [`add_w()`] once
#'   the raw data is joined with the appropriate set of tin tares from
#'   [`asi468_tin_tares`].
#'
#' @return numeric vector (length 1) containing the liquid limit as a decimal
#' @export
#'
#' @example /inst/examples/compute_LL_example.R
#'
#' @references [ASTM D4318 - 17e1](https://www.astm.org/Standards/D4318)
#'
compute_LL <- function(df) {

  curve <- stats::na.omit(stats::lm(data = df, formula = water_content ~ log(blow_count)) )

  LL <- stats::predict.lm(object = curve, newdata = data.frame(blow_count= c(25)) )

}
