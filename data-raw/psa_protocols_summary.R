## code to prepare `psa_protocols_summary` dataset goes here


# This data object is built independtly from the more verbose data object
# `psa_protocols`, but they have similar dependencies.....the former is designed
# for easy use with tab completion, while this one is meant for a quick lookup
# of the number.

library(tidyverse, quietly = T)

# make a tibble containing the summaries of each protocol
# note that protocl_ID is a character type even though I am using numbers
# this is to allow the protocols to each be named list objects
# because object names cannot be numeric type.

psa_protocols_summary <- readr::read_csv(here::here("inst/lab_protocols/particle_size_analysis/psa-methods-terse.csv"),
                                      col_types = cols(.default = "c", g_sample = col_integer()), na= "-")


# export as a data object

usethis::use_data(psa_protocols_summary, overwrite = TRUE)
