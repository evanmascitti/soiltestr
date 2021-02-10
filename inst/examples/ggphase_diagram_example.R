# make a diagram for each of the 5 samples in a Proctor test # note the
# subtle changes in the solid phases volume and the more severe change in
# water-filled pore space
library(magrittr)

compaction_data <- soiltestr::example_proctor_data %>%
  soiltestr::add_physical_properties()

standard_data <-
  dplyr::filter(compaction_data, compaction_effort == "standard")

args <- standard_data %>%
  dplyr::select(ambient_temp_c, water_content, dry_density) %>%
  dplyr::mutate(
    sand_pct = 0.6,
    clay_pct = 0.4,
    G_sa = 2.67,
    G_c = 2.78,
    labels = TRUE,
    values = FALSE
  )

plots <- purrr::pmap(args, ggphase_diagram)

patchwork::wrap_plots(plots, nrow = 1)
