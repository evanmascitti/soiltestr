#' Particle size-analysis protocols
#'
#' Detailed information about specimen pretreatement, dispersion,
#' and measurements.
#'
#' @details Each list item contains details about how the test was performed. Called
#' internally by [`psa()`] and useful for reference during data analysis and for
#' reproducibility of results by different operators.
#'
#' This list can be easily extended because each protocol has a unique ID.
#' Each ID (i.e. object name) is a number enclosed in backticks. This allows
#' unlimited expansion (letters could conceivably be exhausted).
#'
#' @format If **tibble** or **dplyr** is loaded, a tibble with two list-columns
#'   (`extra_pretreatement` and `references`). Otherwise, a data frame with
#'   list-columns.
#'
#'
"psa_protocols"
