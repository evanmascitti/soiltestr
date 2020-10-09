#' The observations include the tin with the wet sample, the tin with the dry
#' sample, and the tin tare. In normal circumstances the tin tare would be
#' looked up using a `left_join` and another data frame of tin numbers and their
#' known masses.
#'
#'@format A tibble with 6 rows and 4 variables:
#'\describe{
#'\item{tin_w_wet_sample}{mass of tin + mass of wet sample}
#'\item{tin_w_OD_sample}{mass of tin + mass of oven-dry ("OD") sample}
#'\item{tin_number}{the number assigned to the aluminum weighing tin}}
#'
"example_wcont"