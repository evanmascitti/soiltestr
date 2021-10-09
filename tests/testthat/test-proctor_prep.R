test_that("Proctor prep returns the right aliquot", {

  # copied from the examples file

  w_extants <- tibble::tibble(
    sample_name = paste0("mix_0", 1:3),
    w_extant = c(0.048, 0.027, 0.052),
  )

  pl_values <- tibble::tibble(
    sample_name = w_extants$sample_name,
    PL = c(0.095, 0.10, 0.14))

  efforts <- tidyr::crossing(
    sample_name = paste0("mix_0", 1:3),
    effort = c('reduced', 'standard', 'modified')
  )



  full_args <- list(efforts, w_extants, pl_values) %>%
    purrr::reduce(dplyr::left_join, by= 'sample_name')

  # now pass the complete table to `proctor_prep()`
  all_efforts_aliquots <- proctor_prep(x = full_args)

  # An alternative approach is to provide the x argument (which can be piped directly from `sand_clay_mix_calcs()` _and_ a data frame
  # having PL values

  component_masses <- sand_clay_mix_calcs(
    mix_date = Sys.Date(),
    sample_name = NULL,
    sandy_name = "concrete-sand",
    clayey_name = 'black-gumbo',
    final_sand_pct = seq(0.2, 0.8, by = 0.2),
    final_OD_kg = 30,
    sand_sandy = 0.97,
    sand_clayey = 0.06,
    w_sand = 0.001,
    w_clay = 0.02)

  pl_df2 <- tibble::tibble(
    sample_name = component_masses$sample_name,
    PL = (1 - 0.01*component_masses$final_sand_pct) * 0.3
  )

  black_gumbo_aliquots <- proctor_prep(component_masses, PLs = pl_df2)


  # Alternatively, provide individual 'loose" vectors
  individual_vectors_approach_aliquots <- proctor_prep(sample_name = w_extants$sample_name,
               w_extant = w_extants$w_extant,
               PLs = pl_values$PL)

  mix_03 <- dplyr::filter(full_args, sample_name == 'mix_03', effort == 'standard')
  # widen the interval for a clayey soil
  wider_intervals_aliquots <- proctor_prep(x = mix_03, PLs = pl_values,
               date= Sys.Date(), w_int = 0.025)




# # the actual tests come here --------------------------------------------

# First for the approach of providing a complete table containing  plastic limit values in addition
  # to compaction test info

# make sure all effort x soil combinations are included
  expect_equal(
    object = nrow(all_efforts_aliquots),
    expected = 54
  )

  expect_equal(
    object = all_efforts_aliquots$water_to_add_g,
    expected = c(-50, -20, 20, 50, 90, 120, 10, 40, 70, 110, 140, 170, 20, 50, 80, 120, 150, 190, 60, 90, 120, 160, 190, 220, 120, 150, 190, 220, 250, 290, 170, 210, 240, 270, 310, 340, 0, 40, 70, 100, 140, 170, 60, 100, 130, 160, 200, 230, 100, 130, 160, 200, 230, 260)
  )


# Wider interval using recommended approach ----------------------------------------------------------

  resulting_wider_range_w_int <- wider_intervals_aliquots %>%
    dplyr::mutate(computed_w_int = w_target - dplyr::lag(w_target)) %>%
    dplyr::pull('computed_w_int') %>%
    round(digits = 4)


expect_equal(
  object = unique(resulting_wider_range_w_int[!is.na(resulting_wider_range_w_int)]),
  expected = 0.025
)


# using two separate data frames, one for test info and
# one for PL values

expect_equal(
  object = black_gumbo_aliquots$w_target,
  expected = c(0.135, 0.15, 0.165, 0.18, 0.195, 0.21, 0.09, 0.105, 0.12, 0.135, 0.15, 0.165, 0.045, 0.06, 0.075, 0.09, 0.105, 0.12, 0, 0.015, 0.03, 0.045, 0.06, 0.075, 0.195, 0.21, 0.225, 0.24, 0.255, 0.27, 0.135, 0.15, 0.165, 0.18, 0.195, 0.21, 0.075, 0.09, 0.105, 0.12, 0.135, 0.15, 0.015, 0.03, 0.045, 0.06, 0.075, 0.09),
  tolerance = 0.0001
)

# passing individual vectors
# this prouces some negative values, indicating the soil needs to dry for at least one of the aliquots
expect_equal(
  object = individual_vectors_approach_aliquots$water_to_add_g,
  expected = c(-50, -20, 20, 50, 90, 120, 10, 40, 70, 110, 140, 170, 20, 50, 80, 120, 150, 190, 0, 40, 70, 100, 140, 170, 60, 100, 130, 160, 200, 230, 100, 130, 160, 200, 230, 260)
  )



})
