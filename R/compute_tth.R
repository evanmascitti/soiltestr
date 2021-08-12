#' Calculate 2-theta angle for XRD reflection
#'
#' Accepts a d-spacing and reflection order
#'
#' @param d d-spacings in Angstroms
#' @param n order of the reflection.
#' @param lambda wavelength of X-ray radiation, defaults to
#' K~alpha~ for Cu radiation (1.54056)
#'
#' @return two-theta angle in degrees
#' @export
#'
compute_tth <- function(d, n = 1, lambda = 1.54056){

  # browser()

  # theta_values_in_rad <- purrr::map_dbl(
  #   n,
  #   .f = ~circular::deg(asin((.x * lambda)/(2 * d)) ))


  theta <- circular::deg(asin((n * lambda)/(2 * d)) )

  tth <- 2 * theta

  return(tth)

}
