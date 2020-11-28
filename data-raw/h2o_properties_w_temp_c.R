## code to prepare `water_densities_w_temp` dataset goes here

h2o_properties_w_temp_c <- readr::read_csv('inst/extdata/water_props_w_temp_c.csv')

usethis::use_data(h2o_properties_w_temp_c, overwrite = TRUE)

