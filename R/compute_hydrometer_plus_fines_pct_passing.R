#' Calls other internal functions to arrive at a complete data frame having all desired particle diameters
#'
#' @return data frame containing percent passing data
#'
compute_hydrometer_plus_pipette_fines_pct_passing <- function(){

  browser()

  hydrometer_fines_pct_passing <- compute_152H_hydrometer_fines_pct_passing(with_pipette = TRUE)

  pipette_fines_pct_passing <- compute_pipette_fines_pct_passing(with_hydrometer = TRUE)


  complete_fines_pct_passing <- dplyr::bind_rows(hydrometer_fines_pct_passing, pipette_fines_pct_passing)


  return(complete_fines_pct_passing)


}
