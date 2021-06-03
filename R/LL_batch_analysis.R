#' @title Compute the liquid limits for a large run of samples
#'
#' @description Automates the repetitive task of writing a script to calculate
#'   the liquid limits of several specimens.
#'
#' @param dir directory containing the raw data files.
#'
#' @details Function searches for a file containing the string "LL_raw_data."
#'   File is read and analyzed; the original (empty) file should have been
#'   written using [`att_lims_datasheets()`] to ensure compatibility of column
#'   names.
#'
#' @return a tibble containing the liquid limits along with information needed
#'   to uniquely identify each specimen
#' @export
#'
LL_batch_analysis <- function(dir){

  data_file_path <- list.files(path = dir, pattern = "LL[_-]raw[_-]data", full.names = T)

  data_file <- readr::read_csv(data_file_path,
                    col_types = readr::cols(
                      test_type = readr::col_character(),
                      date = readr::col_date(),
                      experiment_name = readr::col_character(),
                      sample_name = readr::col_factor(),
                      batch_sample_number = readr::col_double(),
                      tin_number = readr::col_integer(),
                      blow_count = readr::col_double(),
                      tin_w_wet_sample = readr::col_double(),
                      tin_w_OD_sample = readr::col_double(),
                      tin_tare_set = readr::col_character(),
                      comments = readr::col_character()),
                      na = "-") %>%
    janitor::remove_empty(dat = ., which = "rows")

  specimen_index <- tibble::tibble(
    date = unique(data_file$date),
    experiment_name = unique(data_file$experiment_name),
    sample_name = unique(data_file$sample_name),
    batch_sample_number = unique(data_file$batch_sample_number)
  )

  tin_tare_date <- unique(data_file$tin_tare_set)

  tin_tares <- dplyr::bind_rows(asi468::tin_tares)

  LL_raw_data <- data_file %>%
      dplyr::left_join(tin_tares, by = c('tin_tare_set',
                                         'tin_number')) %>%
      soiltestr::add_w()


  LL_values <- LL_raw_data %>%
  dplyr::group_by(.data$batch_sample_number) %>%
  tidyr::nest() %>%
  dplyr::mutate(
    test_type = "LL",
    LL= purrr::map_dbl(
    .x= .data$data,
    .f= soiltestr::compute_LL),
    water_content = purrr::map_dbl(.data$LL, ~.[[1]]),
    sample_name = purrr::map_chr(.data$data, ~unique(.$sample_name))) %>%
  dplyr::select(.data$batch_sample_number, .data$test_type, .data$water_content) %>%
  dplyr::ungroup() %>%
  dplyr::left_join(specimen_index) %>%
  dplyr::relocate(.data$batch_sample_number:.data$water_content, .after = .data$sample_name)


  # browser()

  labeled_flow_curve <- function(df, title){

    ggflowcurve(df = df )+
      ggplot2::labs(title = title)
  }

# browser()

  plots_df <- LL_raw_data %>%
    dplyr::mutate(sample_name = as.character(sample_name)) %>%
    dplyr::arrange(.data$sample_name) %>%
    dplyr::group_by(.data$batch_sample_number) %>%
    tidyr::nest() %>%
    dplyr::mutate(
      sample_name = purrr::map_chr(data, ~unique(purrr::pluck(., 'sample_name'))),
      plot_title = paste0(
        "Liquid limit test (sample name: ",
        .data$sample_name,
        ")"),
      water_content = purrr::map(data, 'water_content'),
      complete = purrr::map_lgl(water_content, ~!all(is.na(.))))%>%
    dplyr::mutate(
      flow_curve_plot = ifelse(
        complete,
        purrr::map2(.x = .data$data, .y =  .data$plot_title,
                .f = labeled_flow_curve),
        NA
    )
    )


  plots <- plots_df$flow_curve_plot %>%
    purrr::set_names(plots_df$sample_name)



  return(structure(
    list(LL_results = LL_values,
         flow_curve_plots = plots),
    class = 'LL_batch') )

  }



