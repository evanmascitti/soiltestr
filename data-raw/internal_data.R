## code to prepare `internal_data` dataset goes here

# All internal data objects have to be created in this file....
# apparently there can only be one R/sysdata.rda file....I think
# it might be preferred to save multiple .rds files instead of one .rda
# but this is what Hadley recommends so I am going with it


suppressPackageStartupMessages({library(tidyverse)})

# clear workspace of global variables (targeted at my saved values for color hex codes)

rm(list = ls())



# must load existing psa_protocols object into this session to use the object...
# tried a couple ways with sourcing the files that create them then removing all
# objects except those matching the pattern psa_protocols.*; finally dawned on
# me to just load the rds objects into the session and assign them the correct names! (duh)


load('./data/psa_protocols.rda')
load('./data/psa_protocols_summary.rda')



###########################

# create vector for use in regex below
possible_psa_fines_method_abbreviations <- paste(
  "hydrometer-plus-pipette",
  "pipette",
  "hydrometer",
  "laser-diffraction",
  sep = "|"
)

# create tibble object containing terse versions of fines methods
psa_fines_methods <- tibble::enframe(psa_protocols,
                                     name = "protocol_ID",
                                     value = "protocol_info") %>%
  dplyr::mutate(fines_method = purrr::map_chr(protocol_info, ~.$fines_method)) %>%
  dplyr::mutate(fines_method = c(stringr::str_extract(fines_method, stringr::regex(possible_psa_fines_method_abbreviations, ignore_case = TRUE)))) %>%dplyr::select(.data$protocol_ID, .data$fines_method)

# create vectors which select the correct protocol ID
# based on which fines method they use

pipette_invoking_protocol_IDs <- unlist(psa_fines_methods[str_detect(psa_fines_methods$fines_method, "pipette"),  'protocol_ID']) %>%
  .[!is.na(.)]
message("pipette_invoking_protocol_IDs are ", pipette_invoking_protocol_IDs, "\n", sep = "\n")


hydrometer_invoking_protocol_IDs <- unlist(psa_fines_methods[str_detect(psa_fines_methods$fines_method, "hydrometer"),  'protocol_ID']) %>%
  .[!is.na(.)]

cat("hydrometer_invoking_protocol_IDs are ", hydrometer_invoking_protocol_IDs, "\n", sep = "\n")

dual_fines_method_invoking_protocol_IDs <- psa_fines_methods[stringr::str_detect(psa_fines_methods$fines_method, "hydrometer") & stringr::str_detect(psa_fines_methods$fines_method, "pipette"), ]$protocol_ID %>%
  .[!is.na(.)]
cat("dual_fines_method_invoking_protocol_IDs are ", dual_fines_method_invoking_protocol_IDs, "\n", sep = "\n")


fines_laser_diffraction_invoking_protocol_IDs <- unname(unlist(psa_fines_methods[psa_fines_methods$fines_method == 'laser-diffraction', 'protocol_ID'])) %>%
  .[!is.na(.)]

cat("fines_laser_diffraction_invoking_protocol_IDs are ", fines_laser_diffraction_invoking_protocol_IDs, "\n", sep = "\n")



# create tibble object containing terse versions of coarse methods --------


psa_coarse_methods <- tibble::enframe(psa_protocols,
                                      name = "protocol_ID",
                                      value = "protocol_info") %>%
  dplyr::mutate(coarse_method = purrr::map_chr(protocol_info, ~.$coarse_method)) %>%
  dplyr::mutate(coarse_method = c(stringr::str_match(coarse_method, stringr::regex(pattern = "shaken|mastersizer", ignore_case = T)))) %>%
  dplyr::mutate(coarse_method = stringr::str_to_lower(coarse_method)) %>%
  dplyr::select(-protocol_info)


# make a few other character vectors based on conditions

sieve_invoking_protocol_IDs <- psa_coarse_methods[psa_coarse_methods$coarse_method == "shaken", , drop = FALSE ]$protocol_ID

coarse_laser_diffraction_invoking_protocol_IDs <- unlist( psa_coarse_methods[psa_coarse_methods$coarse_method %in% c('mastersizer-3000') , 'protocol_ID'])

pretreatment_invoking_protocol_IDs <- psa_protocols_summary %>%
  dplyr::select(c(protocol_ID, dplyr::contains('removal'))) %>%
  dplyr::filter(dplyr::if_any(.cols = contains('removal'),
                .fns = ~!is.na(.))) %>%
  purrr::pluck('protocol_ID')
cat('pretreatment_invoking_protocol_IDs are', pretreatment_invoking_protocol_IDs, sep = "\n")

# the ones that can the **fines** broken out into sub-classes
# must have at least 3 measurements of the fines diameters

fines_sub_bin_invoking_protocol_IDs <-psa_protocols %>%
  map("fines_diameters_sampled") %>%
  flatten() %>%
  keep(~length(.) >= 3)  %>%
  names()

cat('fines_sub_bin_invoking_protocol_IDs are', fines_sub_bin_invoking_protocol_IDs, sep = "\n")

# for the coarse measurements, same concept except there must be **more**
# than 3 because 3 is just gravel and the upper and lower limits to be considered
# sand

coarse_sub_bin_invoking_protocol_IDs <-psa_protocols %>%
    map("coarse_diameters_sampled") %>%
    flatten() %>%
    keep(~length(.) > 3)  %>%
    names()
cat('coarse_sub_bin_invoking_protocol_IDs are', coarse_sub_bin_invoking_protocol_IDs, sep = "\n")
##############################################################################


# determine whether the blank correction for the hydrometer will be
# made via companion measurements or a calibration curve

# (not yet done )


# setting equipment parameters --------------------------------------------

# this is a generic error message used for informing the user about
# how to set the values for equipment dimensions

equipment_instructions <- function(equipment_type) {

  stop(
  "No lookup table found for", paste0("`", equipment_type, "`"), ".", "\n",
  "Please provide a data frame as an argument in the call. \nAlternatively, set a global option at the top of your script, or your .Rprofile file.\n",
  "Editing your .Rprofile is easy with `usethis::edit_r_profile()`.\n",
  "If you do not know what an .Rprofile file is, see https://support.rstudio.com/hc/en-us/articles/360047157094-Managing-R-with-Rprofile-Renviron-Rprofile-site-Renviron-site-rsession-conf-and-repos-conf.",
  call. = F)


}

# identify protocols that wash the sample through the #270 sieve
# _after_ pipette or hydrometer sampling

after_fines_sampling_wash_through_protocol_IDs <- psa_protocols_summary %>%
  dplyr::filter(wash_thru_270_time == 'after') %>%
  purrr::pluck("protocol_ID")
cat('after_fines_sampling_wash_through_protocol_IDs are', after_fines_sampling_wash_through_protocol_IDs, sep = "\n")

before_fines_sampling_wash_through_protocol_IDs <- psa_protocols_summary %>%
  dplyr::filter(wash_thru_270_time == 'before') %>%
  purrr::pluck("protocol_ID")
cat('before_fines_sampling_wash_through_protocol_IDs are', before_fines_sampling_wash_through_protocol_IDs, sep = "\n")


# identify protocols that use a wash-through procedure for computing total fines

wash_through_protocol_IDs <- psa_protocols_summary %>%
  dplyr::filter(fines_method == "wash-through") %>%
  purrr::pluck("protocol_ID")
cat('\n wash_through_protocol_IDs are', wash_through_protocol_IDs, sep = "\n")



ultrasonics_protocol_IDs <- psa_protocols_summary %>%
  dplyr::filter(
    str_detect(mechanical_dispersion, "ultrasonics")) %>%
  purrr::pluck("protocol_ID")

message("\n ultrasonics_protocol_IDs are \n", paste0(ultrasonics_protocol_IDs, "\n"))


# collect and export ------------------------------------------------------


# collect all objects in the global  environment into a list
internal_data <- mget(x = ls())

# write the list to the internal data file

usethis::use_data(internal_data, overwrite = TRUE, internal = TRUE)
