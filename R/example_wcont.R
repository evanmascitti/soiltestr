#' Typical soil water contents near the plastic limit
#'
#' The observations include the tin with the wet sample, the tin with the
#' oven-dry sample, and the tin tare. In typical usage the tin tare would be
#' looked up using a `left_join` and another data frame containing the tin
#' numbers and their known masses. This step is left to the user; otherwise the
#' tins can be manually weighed and included in the raw data before it is
#' imported to R.
#'
#'@format A tibble with 6 rows and 4 variables:
#'
#'\describe{
#'   \item{tin_w_wet_sample}{mass of tin + mass of wet sample}
#'   \item{tin_w_OD_sample}{mass of tin + mass of oven-dry ("OD") sample}
#'   \item{tin_number}{the number assigned to the aluminum weighing tin}
#'}
#'
"example_wcont"
