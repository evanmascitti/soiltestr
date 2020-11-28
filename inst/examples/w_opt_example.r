std_eff <- dplyr::filter(example_proctor_data, compaction_effort == 'standard')
w_opt(std_eff, specific_gravity = Gs)
