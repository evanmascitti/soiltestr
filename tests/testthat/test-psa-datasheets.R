test_that("6 files are written for protocol 7", {


  # if files are for some reason left over from a prior test, remove them
  unlink(x = here::here('psa-data_2021-04-18'), recursive = TRUE)


  # write sheets  to disk


  psa_datasheets(dir = here::here(),
                 date = "2021-04-18",
                 protocol_ID = "7",
                 experiment_name = "my-experiment",
                 sample_names = 1:4,
                 n_reps = 3,
                 beaker_tare_set = "2021-01-15",
                 pipette_beaker_numbers = 1:12
                 )



  # check how many are there

  n_files <- length(list.files(path = here::here('psa-data_2021-04-18'), recursive = T) )

  expect_equal(n_files, 6)


  # remove files to clean up
  unlink(x = here::here('psa-data_2021-04-18'), recursive = TRUE)

  })



