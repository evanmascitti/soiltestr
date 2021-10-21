test_that("Quartz sand returns Gs of 2.65 +/- 0.01", {

  x <- Gs_batch_analysis(
    file = here::here("tests/testthat/test-data/specific-gravity/specific-gravity-data_2021-10-21.csv"),
    pycnometer_bottles = asi468::volumetric_flasks,
    tin_tares = asi468::tin_tares
  )

  # verify that mean is computed correctly from the 2 replications
  expect_equal(
  unlist(x$Gs_avg_values$Gs),
  2.65,
  tolerance = 0.01
)

  # check that the value is computed for both samples
  expect_equal(
    nrow(x$Gs_all_values),
    2
  )
})
