


test_that("PSA protocol 3", {

  # set options for equipment

  options(soiltestr.bouyoucos_cylinder_dims = asi468::bouyoucos_cylinders,
          soiltestr.tin_tares = asi468::tin_tares,
          soiltestr.hydrometer_dims = asi468::astm_152H_hydrometers,
          soiltestr.beaker_tares = asi468::psa_beaker_tares)



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
    dplyr::mutate(across(c(replication, batch_sample_number), .fns = as.factor)) %>%
    tidyr::pivot_longer((where(is.numeric)), values_to =  'percent_in_bin') %>%
    dplyr::group_by(across(where(is.factor))) %>%
    dplyr::summarize(percent_in_bin = sum(percent_in_bin), .groups = 'drop') %>%
    purrr::pluck("percent_in_bin")

  expect_equal(sums, rep(100, 3))

 })


# protocol 8 --------------------------------------------------------------


test_that("PSA protocol 8", {

  psa_obj_8 <- make_test_psa(protocol_ID = 8)

  # check summation to 100% or NULL if sub-bins not computed
  expect_true(psa_summation(psa_object = psa_obj_8,
                            bins_type = "simple"))
  expect_true(psa_summation(psa_object = psa_obj_8,
                            bins_type = "sub"))

  # check names are correct
  expect_equal(names(psa_obj_8),
               psa_names_check())

  # check some extra values for this specific protocol ------------------------------
browser()

  # all 10 values of total sand
  sand_rounded <- round(psa_obj_8$simple_bins$sand, 2)
  expected_sand_values <- c(58.14,
                            57.81,
                            57.41,
                            64.11,
                            52.56,
                            62.16,
                            55.85,
                            63.00,
                            53.77,
                            58.98)
  sand_check <- all(dplyr::near(sand_rounded,
                                expected_sand_values))

  expect_true(sand_check)

  # all 10 values for clay
  clay_rounded <- round(psa_obj_8$simple_bins$clay, 2)
  expected_clay_values <- c(12.06,
                            13.24,
                            13.07,
                            11.80,
                            14.23,
                            12.30,
                            12.75,
                            10.23,
                            13.06,
                            13.61)
  clay_check <- all(dplyr::near(clay_rounded,
                                expected_clay_values))
  expect_true(clay_check)

  })
