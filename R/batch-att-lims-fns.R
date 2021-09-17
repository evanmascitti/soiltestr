#' Analyze plastic limit or adhesion limit data for multiple specimens
#'
#' Called internally by either `AL_batch_analysis()` or `PL_batch_analysis()`
#'
#' @param dir directory containing the raw data file
#' @param type choose AL or PL
#'
#' @details Function searches for a file containing the string "AL-raw-data" or
#'   "PL-raw-data" depending on requested type. File is read and analyzed. The
#'   original (empty) file should have been written using
#'   [`att_lims_datasheets()`] to ensure compatibility of column names.
#'
#' @return a tibble containing the liquid limits along with information needed
#'   to uniquely identify each specimen
#'
#'
AL_or_PL_batch_analysis <- function(type){

 # browser()

  # find the folder from the other function which calls/wraps this one

  # this is the problem, can't figure out why it is looking up one directory level

  dir <- get(x = "dir", envir = rlang::caller_env())

  type <- match.arg(arg = type, choices = c("AL", "PL"))

  # this is a weird way to build the file path but I am having problems
  # with joining the folder and file and this seems to work
  # data_file_path <- fs::path(
  #   dir,
  #   list.files(path = dir, pattern = paste0(type, '[-_]raw[-_]data')))

  data_file_path <- list.files(path = dir,
                               pattern = paste0(type, '[-_]raw[-_]data'),
                               full.names = TRUE)


  # error message if file does not exist

  if(length(data_file_path) == 0) {
    stop(glue::glue("\nNo {type} data file found in this directory."))
  }

  data_file <- readr::read_csv(
    data_file_path,
    col_types = readr::cols(
      test_type = readr::col_character(),
      date = readr::col_date(),
      experiment_name = readr::col_character(),
      sample_name = readr::col_factor(),
      batch_sample_number = readr::col_integer(),
      replication = readr::col_integer(),
      tin_number = readr::col_integer(),
      tin_w_wet_sample = readr::col_character(),
      tin_w_OD_sample = readr::col_character(),
      tin_tare_set = readr::col_character(),
      comments = readr::col_character()
    ),
    na = '-',
    trim_ws = T,
    skip_empty_rows = TRUE,
    lazy = FALSE) %>%
    dplyr::mutate(dplyr::across(
      .cols = dplyr::matches("^tin_w.*sample$"),
      .fns = readr::parse_number))


  n_reps <- length(unique(data_file$replication))

  specimen_index <- tibble::tibble(
    date = unique(data_file$date),
    experiment_name = unique(data_file$experiment_name),
    sample_name = rep(unique(data_file$sample_name), each = n_reps),
    replication = rep(1:n_reps, times = length(unique(data_file$sample_name))),
    batch_sample_number = rep(unique(data_file$batch_sample_number), each = n_reps)
  )

  tin_tare_date <- unique(data_file$tin_tare_set)

# inherit the tin tares data frame from the caller environment

  tin_tares <- get("tin_tares", envir = rlang::caller_env())

 # old version that definitely works but is tied directly to asi468, therefore
  # I'd rather not use
   #
   # tin_tares <- asi468::tin_tares %>%
   #  tibble::enframe(name= "date", value = "tin_tares_data") %>%
   #  tidyr::unnest(.data$tin_tares_data) %>%
   #  dplyr::filter(date == tin_tare_date) %>%
   #  dplyr::select(-.data$date) %>%
   #  dplyr::mutate(tin_number = as.numeric(.data$tin_number))

  consistency_limit_raw_data <- data_file %>%
    dplyr::left_join(tin_tares, by = c("tin_number", "tin_tare_set")) %>%
    soiltestr::add_w()

  consistency_limit_all_values <- consistency_limit_raw_data %>%
    dplyr::mutate(test_type = type) %>%
    dplyr::select(.data$date, .data$experiment_name,
                  .data$sample_name, .data$replication,
                  .data$batch_sample_number, .data$test_type,
                  .data$water_content)


  consistency_limit_avg_values <- consistency_limit_all_values %>%
    dplyr::group_by(dplyr::across(-c(.data$replication, .data$water_content))) %>%
    dplyr::summarize(water_content = mean(.data$water_content, na.rm = TRUE)) %>%
    dplyr::ungroup() %>%
    dplyr::arrange(.data$batch_sample_number)

  consistency_limit_plotting_data <- consistency_limit_avg_values %>%
    tidyr::drop_na() %>%
    dplyr::mutate(sample_name = forcats::fct_reorder(.data$sample_name, .data$water_content))

  consistency_limit_variation_plot <-
    ggplot2::ggplot(
      data = consistency_limit_plotting_data,
      mapping = ggplot2::aes(
        x = .data$water_content,
        y = .data$sample_name,
        color = .data$sample_name
      )
    ) +
    ggplot2::stat_summary(data = consistency_limit_all_values,
                          geom = "errorbarh",
                          fun.data = ggplot2::mean_se,
                          height = 0.2) +
    ggplot2::geom_point(data = consistency_limit_all_values,
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
      expression("Water content, g g" ^ -1 ~ 'x 100'),
      labels = scales::label_percent(accuracy = 1, suffix = ""),
      breaks = scales::breaks_width(width = 0.01, offset = 0)
    ) +
    ggplot2::scale_color_brewer(palette = "Dark2") +
    ggplot2::labs(title = glue::glue("{type} variability across replicate threads"),
                  subtitle = "- Error bars represent standard error of the mean.\n- Small dots represent individual threads; large dot represents mean value.") +
    cowplot::theme_cowplot() +
    ggplot2::theme(axis.line.y = ggplot2::element_blank(),
                   legend.position = 'none')

  return_list <- list(consistency_limit_avg_values,
                     consistency_limit_all_values,
                     consistency_limit_variation_plot) %>%
    purrr::set_names(paste0(
      type, '_', c('avg_values', 'all_values', 'variation_plot')
    ))

  return(return_list)

}

#' Analyze plastic limit data for multiple specimens
#'
#' @param dir folder to look for data file
#' @param tin_tares a data frame of tin tares; if left as NULL, the option `soiltestr.tin_tares` is queried
#' @return List of length 3 (average values, all values, and dot plot of replications)
#' @export
#' @importFrom rlang `%||%`
PL_batch_analysis <- function(dir, tin_tares = NULL){

  tin_tares <- tin_tares %||% getOption('soiltestr.tin_tares') %||% internal_data$equipment_instructions("tin_tares")

  dir <- dir

  results <- AL_or_PL_batch_analysis(type = 'PL')

  return(results)

}


#' Analyze adhesion limit data for multiple specimens
#'
#' @inheritParams PL_batch_analysis
#' @export
#'
AL_batch_analysis <- function(dir, tin_tares = NULL){

  dir <- dir

  tin_tares <- tin_tares %||% getOption('soiltestr.tin_tares') %||% internal_data$equipment_instructions("tin_tares")

  results <- AL_or_PL_batch_analysis(type = 'AL')

  return(results)

}


#' @title Compute the liquid limits for a large run of samples
#'
#' @description Expedites the repetitive task of writing a script to calculate
#'   the liquid limits of several specimens.
#'
#' @param dir directory containing the raw data files.
#' @param tin_tares a data frame of tin tares; if left as NULL, the option `soiltestr.tin_tares` is queried
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
LL_batch_analysis <- function(dir, tin_tares = NULL){

  data_file_path <- list.files(path = dir, pattern = "LL[_-]raw[_-]data", full.names = T)

  data_file <- readr::read_csv(
    data_file_path,
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
      comments = readr::col_character()
    ),
    na = "-",
    trim_ws = TRUE,
    skip_empty_rows = TRUE,
    lazy = FALSE
  )

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
    dplyr::left_join(specimen_index, by = 'batch_sample_number') %>%
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



