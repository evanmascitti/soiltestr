test_that("PSA protocol 3", {

  library(tidyverse)

# set current working directory for clarity and less typing

  setwd(here::here("tests", "testthat", "test-data", "psa", "protocol3"))

  # create psa object
  psa3 <- psa('psa-data_2021-03-04/')


  # check values of each component of the list ------------------------------

  # all 3 values of total sand
  sand_rounded <- purrr::map_dbl(psa3$simple_bins$sand, round, 2)
  expect_equal(object = sand_rounded, expected = c(3.41, 3.57, 3.67))

  # all 3 values of fine clay
  expect_equal(object = round(psa3$sub_bins$fine_clay, digits = 2), expected = c(27.07, 24.79, 27.14),
               )

  # pretreatment loss percent
  expect_equal(100*round(psa3$averages$pretreatment_loss$pretreatment_loss_pct, 4), 2.65)

  # make sure all plots exist
  expect_equal(length(psa3$psd_plots), 3)

  # all bins add to 100%

  sums <- psa3$sub_bins %>%
    dplyr::select((where(is.numeric))) %>%
    mutate(across(c(replication, batch_sample_number), .fns = as.factor)) %>%
    pivot_longer((where(is.numeric)), values_to =  'percent_in_bin') %>%
    group_by(across(where(is.factor))) %>%
    summarize(percent_in_bin = sum(percent_in_bin), .groups = 'drop') %>%
    pluck("percent_in_bin")

  expect_equal(sums, rep(100, 3))

 })
