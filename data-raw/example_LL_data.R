## code to prepare `example_LL_data` dataset goes here

example_LL_data <- readr::read_csv(
  'inst/extdata/example_LL_raw_data.csv',
  lazy = FALSE)

usethis::use_data(example_LL_data, overwrite = TRUE)
