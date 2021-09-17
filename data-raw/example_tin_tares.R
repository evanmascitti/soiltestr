## code to prepare `example_tin_tares` dataset goes here

library(magrittr)

example_tin_tares <- list.files('inst/extdata/example_tin_tares/', pattern  = 'csv', full.names = T) %>%
  purrr::set_names(stringr::str_remove(string = basename(.), pattern = ".csv")) %>%
  purrr::map(readr::read_csv, lazy = FALSE)


usethis::use_data(example_tin_tares, overwrite = TRUE)
