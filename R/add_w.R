#' add_w
#'
#' @param df A data frame containing columns "tin_w_wet_sample", "tin_w_OD_sample", and "tin_tare".
#'
#' @return A new data frame containing the original data with an added column, "water_content".
#'
#' @export
#'
#' @examples
add_w <- function(df) {
  df$water_content <- (df$tin_w_wet_sample - df$tin_w_OD_sample) / (df$tin_w_OD_sample - df$tin_tare)
  return(df)
}

