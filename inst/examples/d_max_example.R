std_eff <- dplyr::filter(example_proctor_data, compaction_effort == 'standard')
d_max(std_eff, specific_gravity = Gs)
