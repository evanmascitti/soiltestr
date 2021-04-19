test_that("errors thrown when argument lengths don't match", {

  # if beaker numbers are supplied, they have to be the same
  # length as the number of reps x number of samples x number of
  # particle diameters tested


  # too many beakers c.f. number of pipette samples to take; protocol 7
  # only samples for 2 microns; beaker vector should be of length 12
  expect_error(
    psa_datasheets(
      dir = here::here(),
      date = Sys.Date(),
      protocol_ID = "7",
      experiment_name = "my-experiment",
      sample_names = 1:4,
      n_reps = 3,
      beaker_tare_set = "2021-01-15",
      pipette_beaker_numbers = 1:24
    )
  )

  # see above;
  # not enough beakers c.f. number of pipette samples to take;
  # protocol 1 samples for 4 sizes; should be 48 beakers
  expect_error(
    psa_datasheets(
      dir = here::here(),
      date = Sys.Date(),
      protocol_ID = "1",
      experiment_name = "my-experiment",
      sample_names = 1:12,
      n_reps = 1,
      beaker_tare_set = "2021-01-15",
      pipette_beaker_numbers = 1:36
    )
  )

  # more samples than Bouyoucos cylinders
  expect_error(
    psa_datasheets(
      dir = her::here(),
      date = Sys.Date(),
      experiment_name = 'my-experiment',
      sample_names = 1:5,
      n_reps = 3,
      protocol_ID = "7",
      tin_tare_set = '2021-03-04',
      beaker_tare_set = '2021-01-15',
      bouyoucos_cylinder_numbers = 1:12,
    )
  )

})


