test_that("w_extant computation accuracy", {

  x <- w_addition(OD_mass = 2000, w_extant = 0.05, w_final = 0.18)

  expect_equal(
    x$air_dry_soil_to_use,
    2100,
    tolerance = 0.1
    )

  expect_equal(
    x$water_to_add,
    260,
    tolerance = 0.1
  )
})
