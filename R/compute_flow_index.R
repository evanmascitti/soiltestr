#'
#' @title Calculate the flow index from the flow curve of a series of LL tests
#'
#'
#' @inheritParams LL_batch_analysis
#' @return tibble containing sample names and flow indices
#' @export
#'
#' @references [ASTM D4318 - 17e1](https://www.astm.org/Standards/D4318)
#'
compute_flow_index <- function(dir, tin_tares = NULL) {



  #  copied from LL batch analysis

  data_file_path <- list.files(path = dir, pattern = "LL[_-]raw[_-]data", full.names = T)

  if(length(data_file_path) == 0L){

    message("No LL data file found in directory ", dir, ". Returning empty data frames and plots for this data collection date.")

    return(structure(
      list(LL_results = NULL,
           flow_curve_plots = NULL),
      class = 'LL_batch') )

  }


  # everything below applies if the file does exist


  #browser()

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

  tin_tares <-  tin_tares %||% getOption(x = "soiltestr.tin_tares") %||% internal_data$equipment_instructions('tin_tares')

  LL_raw_data <- data_file %>%
    dplyr::left_join(tin_tares, by = c('tin_tare_set',
                                       'tin_number')) %>%
    soiltestr::add_w()

  # browser()

  flow_indices <- LL_raw_data %>%
    dplyr::group_by(.data$batch_sample_number) %>%
    tidyr::nest() %>%
    dplyr::mutate(
      flow_curve_model = purrr::map(
        data,
        fit_flow_curve_model
      ),
      flow_index_coefficients = purrr::map(flow_curve_model, "coefficients"),
      flow_index = purrr::map_dbl(flow_index_coefficients, 2),
      sample_name = purrr::map_chr(.data$data, ~ unique(.$sample_name))) %>%
    dplyr::select(.data$batch_sample_number, .data$flow_index) %>%
    dplyr::ungroup() %>%
    dplyr::left_join(specimen_index, by = 'batch_sample_number') %>%
    dplyr::select(.data$sample_name, .data$flow_index)


  return(flow_indices)
}



#' Helper for calculating flow index.
#'
#' Deals with samples having NA values for all cases
#'
#' @param x
#'
#' @return data frame OR an NA value
#'
fit_flow_curve_model <- function(x){


  # browser()

  if(all(is.na(x$water_content))){
    return(NULL)
  } else{
    flow_curve_model <- stats::na.omit(
      stats::lm(data = x,
                formula = water_content ~ log(blow_count)))

    return(flow_curve_model)
  }

}

