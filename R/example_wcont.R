#' Typical soil water contents near the plastic limit
#'
#' The observations include the tin with the wet sample, the tin with the
#' oven-dry sample, and the tin tare. In typical usage the tin tare would be
#' looked up using a `left_join` and a data frame contained in `example_tin_tares`.
#'
#'@format A tibble with 6 rows and 4 variables:
#'
#' - `tin_w_wet_sample`:mass of tin + mass of wet sample
#' - `tin_w_OD_sample`: mass of tin + mass of oven-dry ("OD") sample
#' - `tin_number`: the number assigned to the aluminum weighing tin
#'
#'
"example_wcont"
