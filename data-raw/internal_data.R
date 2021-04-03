## code to prepare `internal_data` dataset goes here

# All internal data objects have to be created in this file....
# apparently there can only be one R/sysdata.rda file....I think
# it might be preferred to save multiple .rds files instead of one .rda
# but this is what Hadley recommends so I am going with it


rm(list = ls())

library(tidyverse)

# clear workspace of global variables (targeted at my saved values for color hex codes)


# create tibble object containing terse versions of fines methods

psa_fines_methods <- tibble::enframe(soiltestr::psa_protocols,
                                     name = "protocol_ID",
                                     value = "protocol_info") %>%
  dplyr::mutate(fines_method = purrr::map_chr(protocol_info, ~.$fines_method)) %>%
  dplyr::mutate(fines_method = c(stringr::str_match(fines_method, "pipette|hydrometer|laser[\\s_-]diffraction"))) %>%
  dplyr::select(-protocol_info)

# create vectors which select the correct protocol number
# based on which fines method they use

pipette_invoking_method_IDs <- psa_fines_methods[psa_fines_methods$fines_method == "pipette", ]$protocol_ID

hydrometer_invoking_method_IDs <- psa_fines_methods[psa_fines_methods$fines_method == "hydrometer", ]$protocol_ID

fines_laser_diffraction_invoking_method_IDs <- psa_fines_methods[psa_fines_methods$fines_method == "laser diffraction", ]$protocol_ID

# create tibble object containing terse versions of coarse methods --------


psa_coarse_methods <- tibble::enframe(soiltestr::psa_protocols,
                                      name = "protocol_ID",
                                      value = "protocol_info") %>%
  dplyr::mutate(coarse_method = purrr::map_chr(protocol_info, ~.$coarse_method)) %>%
  dplyr::mutate(coarse_method = c(stringr::str_match(coarse_method, stringr::regex(pattern = "shaken|mastersizer", ignore_case = T)))) %>%
  dplyr::mutate(coarse_method = stringr::str_to_lower(coarse_method)) %>%
  dplyr::select(-protocol_info)

sieve_invoking_method_IDs <- psa_coarse_methods[psa_coarse_methods$coarse_method == "shaken", ]$protocol_ID

coarse_laser_diffraction_invoking_method_IDs <- psa_fines_methods[psa_fines_methods$fines_method == "mastersizer", ]$protocol_ID

internal_data <- mget(x = ls())

usethis::use_data(internal_data, overwrite = TRUE, internal = TRUE)
