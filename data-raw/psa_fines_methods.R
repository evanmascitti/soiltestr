## code to prepare `psa_fines_methods` dataset goes here
library(magrittr)

psa_fines_methods <- tibble::enframe(soiltestr::psa_protocols,
                                 name = "protocol_ID",
                                 value = "protocol_info") %>%
  dplyr::mutate(fines_method = purrr::map_chr(protocol_info, ~.$fines_method)) %>%
  dplyr::mutate(fines_method = c(stringr::str_match(fines_method, "pipette|hydrometer|laser[\\s_-]diffraction"))) %>%
  dplyr::select(-protocol_info)

usethis::use_data(psa_fines_methods, overwrite = TRUE, internal = T)
