## code to prepare `internal_data` dataset goes here

# All internal data objects have to be created in this file....
# apparently there can only be one R/sysdata.rda file....I think
# it might be preferred to save multiple .rds files instead of one .rda
# but this is what Hadley recommends so I am going with it


library(tidyverse)

# clear workspace of global variables (targeted at my saved values for color hex codes)

rm(list = ls())



# must load existing psa_protocols object into this session to use the object...
# tried a couple ways with sourcing the files that create them then removing all
# objects except those matching the pattern psa_protocols.*; finally dawned on
# me to just load the rds objects into the session and assign them the correct names! (duh)


load('./data/psa_protocols.rda')
load('./data/psa_protocols_summary.rda')



###########################
# create tibble object containing terse versions of fines methods
psa_fines_methods <- tibble::enframe(psa_protocols,
                                     name = "protocol_ID",
                                     value = "protocol_info") %>%
  dplyr::mutate(fines_method = purrr::map_chr(protocol_info, ~.$fines_method)) %>%
  dplyr::mutate(fines_method = c(stringr::str_match(fines_method, "pipette|hydrometer|laser[\\s_-]diffraction"))) %>%
  dplyr::select(-protocol_info)

# create vectors which select the correct protocol ID
# based on which fines method they use

pipette_invoking_protocol_IDs <- psa_fines_methods[psa_fines_methods$fines_method == "pipette", ]$protocol_ID

hydrometer_invoking_protocol_IDs <- psa_fines_methods[psa_fines_methods$fines_method == "hydrometer", ]$protocol_ID

fines_laser_diffraction_invoking_protocol_IDs <- psa_fines_methods[psa_fines_methods$fines_method == "laser diffraction", ]$protocol_ID

# create tibble object containing terse versions of coarse methods --------


psa_coarse_methods <- tibble::enframe(psa_protocols,
                                      name = "protocol_ID",
                                      value = "protocol_info") %>%
  dplyr::mutate(coarse_method = purrr::map_chr(protocol_info, ~.$coarse_method)) %>%
  dplyr::mutate(coarse_method = c(stringr::str_match(coarse_method, stringr::regex(pattern = "shaken|mastersizer", ignore_case = T)))) %>%
  dplyr::mutate(coarse_method = stringr::str_to_lower(coarse_method)) %>%
  dplyr::select(-protocol_info)


# make a few other character vectors based on conditions

sieve_invoking_protocol_IDs <- psa_coarse_methods[psa_coarse_methods$coarse_method == "shaken", ]$protocol_ID

coarse_laser_diffraction_invoking_protocol_IDs <- psa_fines_methods[psa_fines_methods$fines_method == "mastersizer", ]$protocol_ID

pretreatment_invoking_protocol_IDs <- psa_protocols_summary %>%
  dplyr::select(c(protocol_ID, dplyr::contains('removal'))) %>%
  dplyr::filter(dplyr::if_any(.cols = contains('removal'),
                .fns = ~!is.na(.))) %>%
  purrr::pluck('protocol_ID')


# the ones that can the **fines** broken out into sub-classes
# must have at least 3 measurements of the fines diameters

(fines_sub_bin_invoking_protocol_IDS <-psa_protocols %>%
  map("fines_diameters_sampled") %>%
  flatten() %>%
  keep(~length(.) >= 3)  %>%
    names())

# for the coarse measurements, same concept except there must be **more**
# than 3 because 3 is just gravel and the upper and lower limits to be considered
# sand

(coarse_sub_bin_invoking_protocol_IDS <-psa_protocols %>%
    map("coarse_diameters_sampled") %>%
    flatten() %>%
    keep(~length(.) > 3)  %>%
    names())

##############################################################################


# determine whether the blank correction for the hydrometer will be
# made via companion measurements or a calibration curve




# collect all objects in the global  environment into a list
internal_data <- mget(x = ls())

# write the list to the internal data file

usethis::use_data(internal_data, overwrite = TRUE, internal = TRUE)
