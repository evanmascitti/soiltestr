## code to prepare `tin_tares` dataset goes here

asi468_tin_tares <- ecmfuns::gather.files('inst/extdata/asi468_tin_tares/', filetype = 'csv') %>%
  purrr::map(readr::read_csv)

usethis::use_data(asi468_tin_tares, overwrite = TRUE)
