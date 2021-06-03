#' \lifecycle{maturing}
#'
#' @title Calculate the liquid limit from the flow curve
#'
#' @description Uses a blow count of 25 to interpolate the liquid limit.
#'
#' @param df data frame containing columns named `water_content` and
#'   `blow_count`. Water contents may be easily computed with [`add_w()`] once
#'   the raw data is joined with the appropriate set of tin tares from
#'   [`example_tin_tares`].
#'
#' @return numeric vector (length 1) containing the liquid limit as a decimal
#' @export
#'
#' @example /inst/examples/compute_LL_example.R
#'
#' @references [ASTM D4318 - 17e1](https://www.astm.org/Standards/D4318)
#'
compute_LL <- function(df) {
#
#   raw_data <- df %>%
#     dplyr::filter(
#       .data$blow_count != "NA",
#       .data$water_content != "NA")

  raw_data <- df[!is.na(df$blow_count) && !is.na(df$water_content), ]

 # if all values are NA just assign an NA value to the LL

  if(nrow(raw_data) == 0L){return(NA)}

  # fit the flow curve
  curve <- stats::na.omit(stats::lm(data = raw_data, formula = water_content ~ log(blow_count)) )

  # calculate the LL from the flow curve liner model
  LL <- stats::predict.lm(object = curve, newdata = data.frame(blow_count= c(25)) )

  return(LL)
}
