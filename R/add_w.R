#' add_w
#'
#' @param df A data frame
#'
#' @return A new data frame with an added column, "water_content"
#' @export
#'
#' @examples
add_w <- function(df) {
  df$water_content <- (df$tin_w_wet_sample - df$tin_w_OD_sample) / (df$tin_w_OD_sample - df$tin_tare)
  return(df)
}

