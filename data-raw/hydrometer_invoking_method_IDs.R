## code to prepare `hydrometer_invoking_protocol_IDs` dataset goes here

hydrometer_invoking_protocol_IDs <- psa_fines_methods[psa_fines_methods$fines_method == "hydrometer", ]$protocol_ID


usethis::use_data(hydrometer_invoking_protocol_IDs, overwrite = TRUE, internal = TRUE)
