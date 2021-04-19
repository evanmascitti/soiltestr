## code to prepare `fines_laser_diffraction_invoking_protocol_IDs` dataset goes here

fines_laser_diffraction_invoking_protocol_IDs <- psa_fines_methods[psa_fines_methods$fines_method == "laser diffraction", ]$protocol_ID

usethis::use_data(fines_laser_diffraction_invoking_protocol_IDs, overwrite = TRUE, internal = TRUE)
