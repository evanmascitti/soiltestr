## code to prepare `water_densities_w_temp` dataset goes here

# a good explanation of the difference between absolute/dynamic viscosity and
# kinematic viscosity; https://physics.info/viscosity/
# Basically, kinematic is the ratio of absolute viscosity to density
# For Stokes' Law, use dyanamic or absolute because the equation also
# accounts for the density in a different part of the equation

library(magrittr)

h2o_properties_w_temp_c <- tibble::tibble(
  water_temp_c = round(seq(0, 100, 0.1),1),
  water_temp_k = 273.15 + water_temp_c,
  A = 2.414*(10^-5),
  B = 247.8,
  C = 140,
  water_absolute_viscosity_Pascal_seconds = A*10^((B/(water_temp_k - C))),
  water_absolute_viscosity_poises = 10*water_absolute_viscosity_Pascal_seconds, # 10x multiplier to convert from true SI units to poises
  water_density_Mg_m3 = 1.000340382 - (7.77*10^-6)*water_temp_c - (4.95*10^-6)*water_temp_c^2,
  water_density_kg_m3 = water_density_Mg_m3*1000) %>%
  dplyr::select(water_temp_c, water_absolute_viscosity_poises,
                water_density_Mg_m3, water_density_kg_m3)


usethis::use_data(h2o_properties_w_temp_c, overwrite = TRUE)

