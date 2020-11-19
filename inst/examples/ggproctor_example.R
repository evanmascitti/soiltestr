# one sample only
standard_dat <- dplyr::filter(example_proctor_data, compaction_effort == "standard")
ggproctor(standard_dat)

# two samples aligned horizontally (not recommended, prefer faceting to preserve curve shapes)
compaction_plots <- example_proctor_data %>%
  dplyr::group_by(compaction_effort) %>%
  tidyr::nest() %>%
  dplyr::mutate(plot = purrr::map(data, ggproctor)) %>%
  .$plot

cowplot::plot_grid(compaction_plots, align = "h", axis = "x")
