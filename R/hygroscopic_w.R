#' Compute hygroscopic water content
#'
#' Works over a batch of samples with or without replications
#'
#' @param dir directory containing hygroscopic water content data
#' @param tin_tares reference to data frame; if NULL (the default), calls `getOption(soiltestr.tin_tares)`
#'
#' @return tibble of sample names and water contents
#' @export
#'
hygroscopic_w <- function(dir, tin_tares = NULL){


  tin_tares <- tin_tares %||% getOption('soiltestr.tin_tares') %||% internal_data$equipment_instructions("tin_tares")

  hygroscopic_file_path <- list.files(
    path = dir,
    pattern = "hygroscopic.*\\.csv$",
    full.names = TRUE
  )

  raw_data <- readr::read_csv(
    hygroscopic_file_path,
    skip_empty_rows = TRUE,
    na = "-",
    col_types = 'Dcciiciddc',
    lazy = FALSE
    )

  hygroscopic_summary <- raw_data %>%
    dplyr::left_join(tin_tares, by = c("tin_tare_set", "tin_number")) %>%
    soiltestr::add_w() %>%
    dplyr::group_by(.data$sample_name) %>%
    dplyr::summarise(water_content = mean(.data$water_content, na.rm = TRUE))


  return(hygroscopic_summary)


}
