test_that("3-way mixtures are computed accurately",{

  setwd(
    here::here(
      'tests', 'testthat', 'test-data', 'mix-calculations'
    )
  )

  # generate a tibble of mix component masses

  sand_data <- readr::read_csv(
    "three-way-mix-calcs-sand-component-data.csv",
    show_col_types = F
  )

  silt_data <- readr::read_csv(
    "three-way-mix-calcs-silt-component-data.csv",
    show_col_types = F
  )

  clay_data <- readr::read_csv(
    "three-way-mix-calcs-clay-components-data.csv",
    show_col_types = F
  )


  x <- sand_w_scr_mix_calcs(
    mix_date = Sys.Date(),
    final_OD_kg = 10,
    sample_name = NULL,
    sandy_name = sand_data$name,
    silty_name = silt_data$name,
    clayey_name = clay_data$name,
    final_sand_pct = 0.6,
    final_scr = 1,
    sand_sandy = sand_data$sand,
    silt_sandy = sand_data$silt,
    clay_sandy = sand_data$clay,
    sand_silty = silt_data$sand,
    silt_silty = silt_data$silt,
    clay_silty = silt_data$clay,
    sand_clayey = clay_data$sand,
    silt_clayey = clay_data$silt,
    clay_clayey = clay_data$clay,
    w_sandy = 0.003,
    w_silty = 0.001,
    w_clayey = 0.025
  )


# check length of output

  expect_equal(object = nrow(x), 2)

  # check sandy component masses
  expect_equal(
    object = x$air_dry_sandy_mass,
    expected = c(6.09, 6.01),
    tolerance = 0.01
  )

  # check silty component masses
  expect_equal(
    object = x$air_dry_silty_mass,
    expected = c(1.27, 0.48)
  )

  # check clayey component masses
  expect_equal(
    object = x$air_dry_clayey_mass,
    expected = c(2.73, 3.61)
  )

  # check new mix water contents
    expect_equal(
    object = x$final_mix_w,
    expected = c(0.009, 0.011),
    tolerance = 0.0001
  )

})
