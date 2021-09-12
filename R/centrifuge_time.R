#' \lifecycle{experimental}
#'
#' Compute particle settling time in a centrifuge
#'
#' Useful for clay mineral separations and more flexible particle size analysis timing
#'
#' @param water_temp_c Water temp in deg. C
#' @param center_to_liquid_surface distance in cm from center of rotation to liquid surface
#' @param sampling_depth depth in cm to insert pipette below liquid surface
#' @param rpm centrifuge revolutions per minuite
#' @param diameter particle diameter (Stokes' ESD), in microns
#' @param Gs specific gravity of soil solids (usually taken as 2.5 g/cm3 due to
#'  non-negligible volume of adsorbed water at particle surface)
#'
#' @return Double of length 1.
#' @export
#'
centrifuge_time <- function(diameter,
                            water_temp_c,
                            center_to_liquid_surface = 7,
                            sampling_depth = 7,
                            rpm = 2000,
                            Gs = 2.7){


# Need to double check all these calcs ------------------------------------


# This is normally used for removing all the silt-sized particles, leaving only
# the fraction < x microns in suspension --------
  # It can also be used to compute settling time for fine clay or for any arbitrary
  # particle ESD.


#  browser()


  # set temperature and viscosity
water_density <- purrr::flatten_dbl(
  h2o_properties_w_temp_c[h2o_properties_w_temp_c$water_temp_c == water_temp_c, 'water_density_Mg_m3']
)


viscosity <- purrr::flatten_dbl(
  h2o_properties_w_temp_c[h2o_properties_w_temp_c$water_temp_c == water_temp_c, 'water_absolute_viscosity_poises']
)



# set the desired parameters for the centrifuge
# and the sampling depth
#
# t= liquid temperature to nearest degree Celsius
# n= kinematic viscosity in poises (kg/m/s)
# s= distance in cm from center of rotation to liquid surface
# sampling_depth_cm = depth to insert pipette
# r= distance in cm from center of rotation to sampling depth
# R= centrifuge RPM
# D= particle Stokes' diameter in microns
# Gs= specific gravity of soil solids
# (taken as 2.5 g/cm3 due to non-negligible
# volume of adsorbed water at particle surface)
# dp= difference in specific gravity between
# soil solids and liquid, in Mg/m3


# calculate the time in minutes to centrifuge
# based on the parameters entered above

r <- center_to_liquid_surface + sampling_depth

centrifuge_t_min <- ((63*10^8)*viscosity*log10(r/ center_to_liquid_surface)) /
  ((rpm^2)*(diameter^2)*(Gs - water_density))

return(centrifuge_t_min)

}






