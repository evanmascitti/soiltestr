

obj <- psa(dir = here::here('tests/testthat/test-data/psa/protocol3/psa-data_2021-03-04/'))




dfs <- obj[c("cumulative_percent_passing", "simple_bins", "sub_bins")]



dfs %>%
  purrr::modify_if(
    .p = ~ "microns" %in% names(.),
    .f = pivot_cumulative_percent_passing_wider) %>%
  purrr::map(summarize_psa2) %>%
  purrr::modify_if(
    .p = ~ any(stringr::str_detect(string = names(.), pattern = "^\\d")),
    .f = pivot_cumulative_percent_passing_longer)
