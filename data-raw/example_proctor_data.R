## code to prepare `example_proctor_data` dataset goes here:

example_proctor_data <- readr::read_csv('data-raw/pro_inf_mix1.csv') %>%
 tibble::as_tibble()

usethis::use_data(example_proctor_data, overwrite = TRUE)
