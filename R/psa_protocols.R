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
#' @format Data frame or tibble containing several list columns.
#'
#'
"psa_protocols"
