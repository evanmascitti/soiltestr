## code to prepare `psa_coarse_methods` dataset goes here
library(magrittr)


psa_coarse_methods <- tibble::enframe(soiltestr::psa_protocols,
                                  name = "protocol_ID",
                                  value = "protocol_info") %>%
  dplyr::mutate(coarse_method = purrr::map_chr(protocol_info, ~.$coarse_method)) %>%
  dplyr::mutate(coarse_method = c(stringr::str_match(coarse_method, stringr::regex(pattern = "shaken|mastersizer", ignore_case = T)))) %>%
  dplyr::mutate(coarse_method = stringr::str_to_lower(coarse_method)) %>%
  dplyr::select(-protocol_info)

usethis::use_data(psa_coarse_methods, overwrite = TRUE, internal = TRUE)
