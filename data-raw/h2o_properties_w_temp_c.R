## code to prepare `water_densities_w_temp` dataset goes here

library(magrittr)

h2o_properties_w_temp_c <- tibble::tibble(
  water_temp_c = seq(0, 100, 0.1),
  water_temp_k = 273.15 + water_temp_c,
  water_absolute_viscosity_poises = (2.414*10^-5)*10^(247.8 / (water_temp_k - 140)),
  water_density_Mg_m3 = 1.000340382 - (7.77*10^-6)*water_temp_c - (4.95*10^-6)*water_temp_c^2,
  water_density_kg_m3 = water_density_Mg_m3*1000)

usethis::use_data(h2o_properties_w_temp_c, overwrite = TRUE)

