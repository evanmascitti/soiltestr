#' Compute sampling depth for pipette analysis
#'
#' Ideally, the same protocol is followed each and every time. In reality,
#' schedules sometimes must be adjusted. This function computes the proper
#' pipette sampling depth for a given time interval after stirring was
#' completed.
#'
#' @param particle_diameter equivalent spherical diameter, microns
#' @param duration_hr hours since stirring (decimal)
#' @param Gs specific gravity of soil particles; defaults to 2.7
#' @param ambient_temp_c measured temperature to nearest 0.1 degree C
#'
#' @return numeric vector of length 1 (centimeters below liquid surface to sample)
#' @export
#'
#' @examples pipette_sample_depth(particle_diameter = 2, duration_hr = 4)
#'
pipette_sample_depth <- function(particle_diameter, duration_hr,
                                 Gs = 2.7, ambient_temp_c = 22){

  water_density <- diRtscience::h2o_properties_w_temp_c %>%
    dplyr::filter(.data$water_temp_c == round(ambient_temp_c, 1)) %>%
    .$water_density_kg_m3 %>%
    .[1]

  water_viscosity <- diRtscience::h2o_properties_w_temp_c %>%
    dplyr::filter(.data$water_temp_c == round(ambient_temp_c, 1)) %>%
    .$water_absolute_viscosity_poises %>%
    .[1]

  gravity <- 9.80665

  sample_depth <- ((duration_hr*60*60*gravity*((particle_diameter*10^-6)^2)*((Gs*1000)-(water_density))) / (18*water_viscosity) )*100

  return(sample_depth)
}
