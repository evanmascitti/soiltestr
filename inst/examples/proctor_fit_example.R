std_eff <- dplyr::filter(example_proctor_data, compaction_effort == 'standard') %>%
  add_physical_properties()
proctor_fit(std_eff)
