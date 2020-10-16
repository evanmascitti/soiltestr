## code to prepare `example_wcont-raw-data` data set goes here

# fix random number generator, strip off extraneous columns,
# remove missing observations, then select 5 rows from the data set to use. Return
# the random number generator to its initial state.

set.seed(10)

example_wcont_data <- readr::read_csv('data-raw/example_wcont-raw-data.csv') %>%
  tibble::as_tibble() %>%
  dplyr::select(tin_w_wet_sample, tin_w_OD_sample, tin_tare) %>%
  tidyr::drop_na() %>%
  dplyr::slice(sample(132, size=5, replace = FALSE))

set.seed(NULL)

example_wcont_data

usethis::use_data(example_wcont_data, overwrite = TRUE)
