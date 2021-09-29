test_that("two-component mixtures are computed correctly", {


  setwd(here::here('tests', 'testthat', 'test-data', 'mix-calculations'))


x <- list.files(pattern = "\\.csv$", full.names = T) %>%
  readr::read_csv(show_col_types = FALSE, lazy = TRUE) %>%
  as.data.frame()


boyd_promound_mix_calcs <- sand_clay_mix_calcs(
  mix_date = '2021-09-28',
  sample_name = NULL,
  sandy_name = x[x$name == 'boyd-20-80', 'name'],
  clayey_name = x[x$name == 'promound', 'name'],
  final_sand_pct = c(0.6, 0.75),
  final_OD_kg = 27,
  sand_sandy = x[x$name == 'boyd-20-80', 'sand'],
  sand_clayey = x[x$name == 'promound', 'sand'],
  w_sandy = 0.001,
  w_clayey = 0.02)


# make sure the calculation runs for both mixes
expect_equal(
  object = nrow(boyd_promound_mix_calcs),
  2
)

# check sandy component air dry mass
expect_equal(
  object = boyd_promound_mix_calcs$kg_air_dry_sandy_component,
  expected = c(16.2162, 20.484),
  tolerance = 0.001
)

# check clayey component air dry mass
expect_equal(
  object = boyd_promound_mix_calcs$kg_air_dry_clayey_component,
  expected = c(11.016, 6.6676),
  tolerance = 0.001
)

})
