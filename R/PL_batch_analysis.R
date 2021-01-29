#' Analyze plastic limit data for multiple specimens
#'
#' @param dir directory containing the raw data file
#'
#' @details Function searches for a file containing the string "PL_raw_data."
#'   File is read and analyzed. The original (empty) file should have been
#'   written using [`att_lims_datasheets()`] to ensure compatibility of column
#'   names.
#'
#' @return a tibble containing the liquid limits along with information needed
#'   to uniquely identify each specimen
#'
#' @export
#'
PL_batch_analysis <- function(dir){

  if(stringr::str_sub(string = dir, start = -2) == "/"){
    directory <- dir} else{
      directory <- paste0(dir, "/")
    }

  data_file_path <- list.files(path = directory,
                               pattern = "PL_raw_data",
                               full.names = T)

  # error message if file does not exist

  if(length(data_file_path) == 0) {
    stop("\nNo plastic limit data file found in this directory.")
  }

  data_file <- suppressMessages(
    readr::read_csv(data_file_path,
                    col_types = cols(
                      test_type = col_character(),
                      date = col_date(),
                      experiment_name = col_character(),
                      sample_name = col_character(),
                      sample_number = col_integer(),
                      replication = col_integer(),
                      tin_number = col_double(),
                      tin_w_wet_sample = col_double(),
                      tin_w_OD_sample = col_double(),
                      tin_tare_set = col_character(),
                      comments = col_character()
                    ))
  )

  n_reps <- length(unique(data_file$replication))

  specimen_index <- tibble::tibble(
    date = unique(data_file$date),
    experiment_name = unique(data_file$experiment_name),
    sample_name = rep(unique(data_file$sample_name), each = n_reps),
    replication = rep(1:n_reps, times = length(unique(data_file$sample_name))),
    sample_number = rep(unique(data_file$sample_number), each = n_reps)
  )

  tin_tare_date <- unique(data_file$tin_tare_set)

  tin_tares <- asi468::tin_tares %>%
    tibble::enframe(name= "date", value = "tin_tares_data") %>%
    tidyr::unnest(.data$tin_tares_data) %>%
    dplyr::filter(date == tin_tare_date) %>%
    dplyr::select(-.data$date) %>%
    dplyr::mutate(tin_number = as.numeric(.data$tin_number))

  PL_raw_data <- readr::read_csv(data_file_path,
                    col_types = cols(
                      test_type = col_character(),
                      date = col_date(),
                      experiment_name = col_character(),
                      sample_name = col_character(),
                      sample_number = col_integer(),
                      replication = col_integer(),
                      tin_number = col_double(),
                      tin_w_wet_sample = col_double(),
                      tin_w_OD_sample = col_double(),
                      tin_tare_set = col_character(),
                      comments = col_character()
                    )) %>%
      dplyr::left_join(tin_tares) %>%
      soiltestr::add_w()

  PL_all_values <- PL_raw_data %>%
    dplyr::rename(PL = .data$water_content) %>%
    dplyr::select(.data$date, .data$experiment_name,
                  .data$sample_name, .data$replication,
                  .data$sample_number, .data$PL)


  PL_values <- PL_raw_data %>%
    dplyr::group_by(.data$sample_number) %>%
    tidyr::nest() %>%
    dplyr::mutate(PL= purrr::map_dbl(.data$data, ~mean(.$water_content),
                                     sample_name = purrr::map_chr(.data$data, ~unique(.$sample_name)))
    )%>%
    dplyr::select(.data$sample_number, .data$PL) %>%
    dplyr::ungroup() %>%
    dplyr::left_join(specimen_index) %>%
    dplyr::relocate(.data$sample_number:.data$PL, .after = .data$sample_name)


  PL_plotting_data <- PL_raw_data %>%
    tidyr::drop_na() %>%
    dplyr::mutate(sample_name = forcats::fct_reorder(.data$sample_name, .data$water_content))

  PL_variation_plot <- ggplot2::ggplot(data= PL_plotting_data,
                                       mapping = ggplot2::aes(x= .data$water_content,
                                                              y= .data$sample_name,
                                                              color = .data$sample_name))+
    ggplot2::stat_summary(geom = "errorbarh", fun.data = ggplot2::mean_se, height = 0.2)+
    ggplot2::geom_point(position = ggplot2::position_jitter(height = 0.2), alpha = 1/4)+
    ggplot2::stat_summary(geom = 'point', fun = mean, size= 4, alpha= 1/2)+
    ggplot2::scale_y_discrete("Sample name")+
    ggplot2::scale_x_continuous(bquote("Plastic limit, % g g"^-1),
                                labels = scales::label_percent(accuracy = 1, suffix = ""),
                                breaks = scales::breaks_width(width = 0.01, offset = 0))+
    ggplot2::scale_color_brewer(palette = "Dark2")+
    ggplot2::labs(title = "Plastic limit variability across replicate threads",
                  subtitle = "- Error bars represent standard error of the mean.\n- Small dots represent individual threads; large dot represents mean value.")+
    cowplot::theme_cowplot()+
    ggplot2::theme(axis.line.y= ggplot2::element_blank(),
                   legend.position = 'none')

  return(list(
    PL_values = PL_values,
    PL_all_values = PL_all_values,
    PL_variation_plot = PL_variation_plot)
  )

}
