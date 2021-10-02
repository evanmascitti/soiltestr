# Recommended approach: provide the `x` argument as a full table of required data

# first prepare three tables - the efforts to test, the existing water contents, and the plastic limits

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

# now pass the complete table to `protor_prep()`
proctor_prep(x = full_args)

# An alternative approach is to provide the x argument (which can be piped directly from `sand_clay_mix_calcs()` _and_ a data frame
# having PL values

component_masses <- sand_clay_mix_calcs(
  mix_date = Sys.Date(),
  sample_name = NULL,
  sandy_name = "concrete-sand",
  clayey_name = 'brown-gumbo',
  final_sand_pct = seq(0.2, 0.8, by = 0.2),
  final_OD_kg = 30,
  sand_sandy = 0.97,
  sand_clayey = 0.06,
  w_sand = 0.001,
  w_clay = 0.02)

pl_df2 <- tibble::tibble(
  sample_name = component_masses$sample_name,
  PL = (1 - 0.01*component_masses$final_sand_pct) * 0.18
)

proctor_prep(component_masses, PLs = pl_df2)


# Alternatively, provide individual 'loose" vectors
proctor_prep(sample_name = w_extants$sample_name,
             w_extant = w_extants$w_extant,
             PLs = pl_values$PL)


# widen the interval for a clayey soil
mix_03 <- dplyr::filter(full_args, sample_name == 'mix_03', effort == 'standard')
proctor_prep(x = mix_03, PLs = pl_values,
             date= Sys.Date(), w_int = 0.025)


