#' Compute soil solids' specific gravity for a batch of samples
#'
#' @param dir folder to look for data file
#' @param pycnometer_bottles a data frame of pycnometer bottle masses and volumes; if left as NULL, the option `soiltestr.pycnometer_bottles` is queried
#' @param tin_tares a data frame of tin tares; if left as NULL, the option `soiltestr.tin_tares` is queried
#'
#' @return a list of length 3: the results for each sample, a summary with the average for each sample (across replications), and a dotplot showing the spread of the results across replications. The column `Gs` is the specific gravity of the soil solids at 20 &deg;C
#' @export
#'
#' @importFrom rlang `%||%`
#'
Gs_batch_analysis <- function(file, pycnometer_bottles = NULL, tin_tares = NULL){

  # find data frames for pycnometer bottles and tin tares

  pycnometer_bottles <- pycnometer_bottles %||% getOption('soiltestr.pycnometer_bottles') %||% internal_data$equipment_instructions("pycnometer_bottles")

  tin_tares <- tin_tares %||% getOption('soiltestr.tin_tares') %||% internal_data$equipment_instructions("tin_tares")

raw_data <- readr::read_csv(
  file = file,
  col_types = 'Dcciicidddcdddc',
  na = "-",
  lazy = FALSE
)

# browser()

Gs_values <- raw_data %>%
  dplyr::left_join(tin_tares, by = c('tin_tare_set', 'tin_number')) %>%
  dplyr::left_join(pycnometer_bottles, by = c('bottle_set', 'bottle_number')) %>%
  dplyr::mutate(water_temp_c = round(water_temp_c, digits = 1)) %>%
  dplyr::left_join(h2o_properties_w_temp_c, by = 'water_temp_c') %>%
  add_w() %>%
  dplyr::mutate(
    OD_soil_mass = air_dry_specimen_mass / (1 + water_content),
    bottle_plus_water_mass = bottle_empty_mass + (bottle_vol * water_density_Mg_m3),
    Gt = OD_soil_mass / (bottle_plus_water_mass - (filled_bottle_post_boil_mass - OD_soil_mass)),
    K = water_density_Mg_m3 / 0.9982063,
    Gs = Gt*K
)


all_results_tbl <- Gs_values[, c("date", "experiment_name", "sample_name", "replication", "batch_sample_number", "Gs")]


summarized_results_tbl <- all_results_tbl %>%
  dplyr::group_by(date, experiment_name, sample_name) %>%
  dplyr::summarise(
    Gs = mean(Gs, na.rm = TRUE)
  ) %>%
  dplyr::ungroup()


# make dotplot of all results

Gs_variation_plot <-
  ggplot2::ggplot(
    data = summarized_results_tbl,
    mapping = ggplot2::aes(
      x = .data$Gs,
      y = .data$sample_name,
      color = .data$sample_name
    )
  ) +
  ggplot2::stat_summary(data = all_results_tbl,
                        geom = "errorbarh",
                        fun.data = ggplot2::mean_se,
                        height = 0.2) +
  ggplot2::geom_point(data = all_results_tbl,
                      position = ggplot2::position_jitter(height = 0.2),
                      alpha = 1 / 4) +
  ggplot2::stat_summary(
    geom = 'point',
    fun = mean,
    size = 3,
    position = position_nudge(y = 0.2),
    alpha = 1 / 2
  ) +
  ggplot2::scale_y_discrete("Sample name") +
  ggplot2::scale_x_continuous(
    expression(G[s]~"Mg m"^3),
    labels = scales::label_number(accuracy = 0.01, suffix = "")) +
  ggplot2::scale_color_brewer(palette = "Dark2") +
  ggplot2::labs(title = expression(G[s]~"variability across replicate determinations"),
                subtitle = "- Error bars represent standard error of the mean.\n- Small dots represent individual measurements; large dot represents mean value across replicates.") +
  ecmfuns::theme_ecm_bw()+
  ggplot2::theme(axis.line.y = ggplot2::element_blank(),
                 legend.position = 'none')



return_list <- list(
  Gs_avg_values = summarized_results_tbl,
  Gs_all_values = all_results_tbl,
  Gs_variation_plot = Gs_variation_plot
)


return(return_list)



}
