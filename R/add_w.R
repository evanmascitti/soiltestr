#' @title Calculate gravimetric water content & add to the data frame
#'
#'@description \lifecycle{stable}
#' The input data should contain
#' columns named `tin_w_wet_sample`, `tin_w_OD_sample`, and `tin_tare`. Note
#' that the tin tare should be looked up from the appropriate set of
#' pre-weighed tins. The formula for computing water content is
#'
#'\loadmathjax
#' \mjdeqn{ \frac{m_{water}}{m_{OD~soil}}}{}
#'
#' @param df A data frame
#'
#' @return A new data frame containing the original data with an added column,
#'   `"water_content"` in decimal form.
#'
#' @export
#'
#' @examples add_w(example_wcont)
#'
#' @references \href{https://www.pearson.com/us/higher-education/product/Brady-Nature-and-Properties-of-Soils-The-13th-Edition/9780130167637.html}{Brady and Weil, 2002. The Nature and Properties of Soil.}

add_w <- function(df) {

  if(!"tin_tare" %in% names(df)){
    stop('\n\n No tin tare found. Did you forget to join the raw data with a set of tin tares?')
  }

  df$water_content <- (df$tin_w_wet_sample - df$tin_w_OD_sample) / (df$tin_w_OD_sample - df$tin_tare)
  return(df)
}


