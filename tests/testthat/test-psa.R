test_that("PSA protocol 3", {

  # set options for equipment

  options(soiltestr.bouyoucos_cylinder_dims = asi468::bouyoucos_cylinders,
          soiltestr.tin_tares = asi468::tin_tares,
          soiltestr.hydrometer_dims = asi468::astm_152H_hydrometers,
          soiltestr.beaker_tares = asi468::psa_beaker_tares)



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

  options(soiltestr.bouyoucos_cylinder_dims = asi468::bouyoucos_cylinders,
          soiltestr.tin_tares = asi468::tin_tares,
          soiltestr.hydrometer_dims = asi468::astm_152H_hydrometers,
          soiltestr.beaker_tares = asi468::psa_beaker_tares)

  setwd(here::here("tests", "testthat", "test-data", "psa", "protocol8"))

  psa_obj_8 <- psa("psa-data_2021-03-30/")

  # commenting out this garbage for now.
  # Functions for checking the data are probably not necessary
  # if each protocol has manually written tests.
  # And I don't have them for
  # every protocol anyway, so it would not be consistently
  # applied even if it were left here .

  # check summation to 100% or NULL if sub-bins not computed
  # expect_true(psa_summation(psa_object = psa_obj_8,
  #                           bins_type = "simple"))
  # expect_true(psa_summation(psa_object = psa_obj_8,
  #                           bins_type = "sub"))

  # check names are correct
  # expect_equal(names(psa_obj_8),
  #              psa_names_check())

  # check some extra values for this specific protocol ------------------------------
# browser()

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


# protocol 15 -------------------------------------------------------------


test_that("PSA protocol 15", {

  # set options for equipment

  options(soiltestr.bouyoucos_cylinder_dims = asi468::bouyoucos_cylinders,
          soiltestr.tin_tares = asi468::tin_tares,
          soiltestr.hydrometer_dims = asi468::astm_152H_hydrometers,
          soiltestr.beaker_tares = asi468::psa_beaker_tares)



  # set current working directory for clarity and less typing

  setwd(here::here("tests", "testthat", "test-data", "psa", "protocol15"))



  # create psa object
  psa15 <- psa(dir = 'psa-data_2021-08-28')



  # check values of each component of the list ------------------------------

  # all 3 values of total sand
  sand_rounded <- purrr::map_dbl(psa15$averages$simple_bins$sand, round, 1)
  expect_equal(object = sand_rounded, expected = c(97.8, 13.4, 4))

  # all 3 values of fine silt
  expect_equal(object = round(psa15$averages$sub_bins$fine_silt, digits = 2),
               expected = c(0.13, 16.79, 11.75))

 # make sure all plots exist
  expect_equal(length(psa15$psd_plots), 12)

  # all bins add to 100%
  # one has missing data, so there should only be 11 values

  sums <- psa15$sub_bins %>%
    dplyr::select((where(is.numeric))) %>%
    dplyr::mutate(across(c(replication, batch_sample_number), .fns = as.factor)) %>%
    tidyr::pivot_longer((where(is.numeric)), values_to =  'percent_in_bin') %>%
    dplyr::group_by(across(where(is.factor))) %>%
    dplyr::summarize(percent_in_bin = sum(percent_in_bin), .groups = 'drop') %>%
    purrr::pluck("percent_in_bin")

  expect_equal(sums[!is.na(sums)], rep(100, 12))

})

# protocol 22 -------------------------------------------------------------


test_that("PSA protocol 22", {

  # set options for equipment

  options(soiltestr.bouyoucos_cylinder_dims = asi468::bouyoucos_cylinders,
          soiltestr.tin_tares = asi468::tin_tares,
          soiltestr.hydrometer_dims = asi468::astm_152H_hydrometers,
          soiltestr.beaker_tares = asi468::psa_beaker_tares)



  # set current working directory for clarity and less typing

  setwd(here::here("tests", "testthat", "test-data", "psa", "protocol22"))



  # create psa object
  psa22 <- psa(dir = 'psa-data_2022-03-28')


  # NEED TO CHANGE THE CODE BELOW SO IT MATCHES THE NEW SAMPLE DATA

  # check values of each component of the list ------------------------------

  # all 3 values of total sand
  sand_rounded <- purrr::map_dbl(psa22$averages$simple_bins$sand, round, 1)
  expect_equal(object = sand_rounded, expected = c(4.7, 50.9))

  # all 3 values of fine silt
  expect_equal(object = round(psa22$averages$sub_bins$fine_silt, digits = 1),
               expected = c(28.6, 25.2))

  # make sure all plots exist
  expect_equal(length(psa22$psd_plots), 2)

  # all bins add to 100%


  sums <- psa22$sub_bins %>%
    dplyr::select((where(is.numeric))) %>%
    dplyr::mutate(across(c(replication, batch_sample_number), .fns = as.factor)) %>%
    tidyr::pivot_longer((where(is.numeric)), values_to =  'percent_in_bin') %>%
    dplyr::group_by(across(where(is.factor))) %>%
    dplyr::summarize(percent_in_bin = sum(percent_in_bin), .groups = 'drop') %>%
    purrr::pluck("percent_in_bin")

  expect_equal(sums[!is.na(sums)], rep(100, nrow(psa22$sub_bins)))

})

# General error checking for all protocols --------------------------------

test_that("Errors stop function calls", {

    options(
      soiltestr.bouyoucos_cylinder_dims = NULL,
      soiltestr.tin_tares = NULL,
      soiltestr.hydrometer_dims = NULL,
      soiltestr.beaker_tares = NULL)

  # no tin tares provided
  expect_error({
    object = psa(
      here::here(
        "tests", "testthat", "test-data", "psa",
        "protocol3", "psa-data_2021-03-04"),
      beaker_tares = asi468::psa_beaker_tares)
    })


  # no beaker tares provided for a pipette method
  expect_error({
    object = psa(
      here::here(
        "tests", "testthat", "test-data", "psa",
        "protocol3", "psa-data_2021-03-04"),
      tin_tares = asi468::tin_tares)
  })

  # wrong set of tin tares in datasheets
  expect_error({
    object = psa(
      here::here(
        "tests", "testthat", "test-data", "psa",
        "general-error-checking", "psa-data_2021-03-04"),
      tin_tares = asi468::tin_tares)
  })

})
