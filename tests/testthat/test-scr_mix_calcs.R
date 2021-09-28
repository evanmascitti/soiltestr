test_that("scr mix of 2 yields correct results", {

  scr2 <- scr_mix_calcs(
    final_OD_kg = 100,
    scr = 2,
    silt_silty = 0.9,
    clay_silty = 0.06,
    silt_clayey = 0.43,
    clay_clayey = 0.54,
    w_silty = 0.001,
    w_clayey = 0.04
  )

  # returned object has 3 elements
  expect_equal(
    object = length(scr2),
    expected = 4)

  # air-dry masses are correct
  expect_equal(
    object = sum(scr2$air_dry_component_masses$air_dry_kg_silty,
                 scr2$air_dry_component_masses$air_dry_kg_clayey),
    expected = 105.9849,
    tolerance = 0.01
  )

  # oven-dry masses sum to 1
  expect_equal(
    sum(scr2$OD_component_ratios),
    1
  )

  # final mix has 3.54% sand by OD mass
  expect_equal(
    object = scr2$final_OD_sand_pct,
    expected = 0.03454545,
    tolerance = 0.0001
  )

  # final mix has water content of 2.2%
  expect_equal(
    object = scr2$w_extant,
    expected = 0.02227273,
    tolerance = 0.0001
  )
})
