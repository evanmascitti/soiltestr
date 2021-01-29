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

  if(stringr::str_sub(string = dir, start = -1) == "/"){
    directory <- dir} else{
      directory <- paste0(dir, "/")
      }

  data_file_path <- list.files(path = directory, pattern = "LL_raw_data", full.names = T)

  data_file <- readr::read_csv(data_file_path,
                    col_types = readr::cols(
                      test_type = readr::col_character(),
                      date = readr::col_date(),
                      experiment_name = readr::col_character(),
                      sample_name = readr::col_character(),
                      sample_number = readr::col_double(),
                      tin_number = readr::col_double(),
                      blow_count = readr::col_double(),
                      tin_w_wet_sample = readr::col_double(),
                      tin_w_OD_sample = readr::col_double(),
                      tin_tare_set = readr::col_character(),
                      comments = readr::col_double()
                    )
                    )

  specimen_index <- tibble::tibble(
    date = unique(data_file$date),
    experiment_name = unique(data_file$experiment_name),
    sample_name = unique(data_file$sample_name),
    sample_number = unique(data_file$sample_number)
  )

  tin_tare_date <- unique(data_file$tin_tare_set)

  tin_tares <- asi468::tin_tares %>%
    tibble::enframe(name= "date", value = "tin_tares_data") %>%
    tidyr::unnest(.data$tin_tares_data) %>%
    dplyr::filter(date == tin_tare_date) %>%
    dplyr::select(-.data$date)

  LL_raw_data <- suppressMessages(
    readr::read_csv(data_file_path,
                    col_types = readr::cols(
                      test_type = readr::col_character(),
                      date = readr::col_date(),
                      experiment_name = readr::col_character(),
                      sample_name = readr::col_character(),
                      sample_number = readr::col_double(),
                      tin_number = readr::col_double(),
                      blow_count = readr::col_double(),
                      tin_w_wet_sample = readr::col_double(),
                      tin_w_OD_sample = readr::col_double(),
                      tin_tare_set = readr::col_character(),
                      comments = readr::col_double()
                    )
    ) %>%
    dplyr::left_join(tin_tares) %>%
    soiltestr::add_w()
  )

  LL_values <- LL_raw_data %>%
  dplyr::group_by(.data$sample_number) %>%
  tidyr::nest() %>%
  dplyr::mutate(LL= purrr::map(
    .x= .data$data,
    .f= purrr::possibly(.f = ~purrr::map(.x= data,
                                         .f = soiltestr::compute_LL),
                        otherwise = NA_real_)),
         LL = purrr::map_dbl(.data$LL, ~.[[1]]),
         sample_name = purrr::map_chr(.data$data, ~unique(.$sample_name))
    ) %>%
  dplyr::select(.data$sample_number, .data$LL) %>%
  dplyr::ungroup() %>%
  dplyr::left_join(specimen_index) %>%
  dplyr::relocate(.data$sample_number:.data$LL, .after = .data$sample_name)

  return(LL_values)

  }



