utils::globalVariables(c('filled_cylinder_mass_g', "empty_cylinder_mass_g", "water_content", "cylinder_vol_cm3",
                         "OD_soil_g", "dry_density", "total_porosity", "."))

std_eff <- dplyr::filter(example_proctor_data, compaction_effort == 'standard')
proctor_fit(std_eff)
