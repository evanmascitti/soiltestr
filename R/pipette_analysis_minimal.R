#' @title Analyze results of a particle size test with pipette and % sand
#'
#' @description For analyses with only one pipette sample and only total % sand
#'   (no sieving)
#'
#' @param dir Directory containing the raw data files
#'
#' @return a tibble containing the sand, silt, and clay % of each specimen along
#'   with metadata. All particles > 2 mm are considered as 'sand' in this method
#'   because no sieving is performed
#' @export
#'
pipette_analysis_minimial <- function(dir){

  # read raw data files

  if(stringr::str_sub(string = dir, start = -1) == "/"){
    directory <- dir} else{
      directory <- paste0(dir, "/")
    }

  test_date <- stringr::str_sub(string = directory, start = -11 , end = -2)

  paths <- list.files(directory, full.names = T) %>%
    basename(.) %>%
    stringr::str_remove(string = ., pattern = paste0("_", test_date, ".csv"))

  datafiles <- suppressMessages(
    list.files(directory, full.names = T) %>%
      purrr::map(readr::read_csv) %>%
      purrr::set_names(paths) %>%
      purrr::map(~dplyr::select(., -comments))
  )

  # change column types to double to account for empty cells containing
  # a dash, which would make them read as character columns
  suppressWarnings(
    datafiles$pipetting_data$beaker_mass_w_OD_sample <- as.double(datafiles$pipetting_data$beaker_mass_w_OD_sample) )

  suppressWarnings(
    datafiles$pipetting_data$beaker_number <- as.integer(datafiles$pipetting_data$beaker_number)
  )

  # make tin tare look-up table from larger data object

  tin_tare_date <-  as.character(unique(datafiles$hygroscopic_correction_data$tin_tare_set))

  tin_tares <- asi468::tin_tares %>%
    tibble::enframe(name= "date", value = "tin_tares_data") %>%
    tidyr::unnest(.data$tin_tares_data) %>%
    dplyr::filter(date == tin_tare_date) %>%
    dplyr::select(-.data$date)


  # make beaker look-up table from larger data object

  beaker_tare_date <- unique(datafiles$pipetting_data$beaker_tare_set)

  beaker_tares <- asi468::psa_beaker_tares %>%
    tibble::enframe(name= "date", value = "beaker_tares_data") %>%
    tidyr::unnest(.data$beaker_tares_data) %>%
    dplyr::filter(date == beaker_tare_date) %>%
    dplyr::select(-.data$date)


  # calculate air-dry water contents

  hygroscopic_water_contents <- suppressMessages(
    datafiles$hygroscopic_correction_data %>%
      dplyr::left_join(tin_tares)%>%
      soiltestr::add_w() %>%
      dplyr::select(-c(.data$tin_tare_set,
                       .data$tin_number,
                       .data$tin_w_wet_sample,
                       .data$tin_w_OD_sample))
  )

  # compute oven-dry specimen masses

  OD_specimen_masses <-
    datafiles$specimen_masses_data %>%
    dplyr::left_join(hygroscopic_water_contents) %>%
    dplyr::mutate(OD_specimen_mass = .data$air_dry_specimen_mass_for_test/(1+ .data$water_content)) %>%
    dplyr::select(-c(.data$air_dry_specimen_mass_for_test,
                     .data$tin_tare,
                     .data$water_content))



  # compute blank correction

  blank_correction <- suppressMessages(
    datafiles$blank_correction_data%>%
      dplyr::left_join(beaker_tares) %>%
      dplyr::mutate(calgon_in_beaker = .data$beaker_mass_w_OD_sample - .data$beaker_empty_mass) %>%
      dplyr::summarize(blank_correction = mean(.data$calgon_in_beaker)) %>%
      purrr::pluck(1)
  )


  # calculate soil solids for each beaker and convert to percent clay

  clay_pcts <- suppressMessages(
    datafiles$pipetting_data %>%
      dplyr::left_join(beaker_tares) %>%
      dplyr::left_join(OD_specimen_masses) %>%
      dplyr::mutate(clay_mass = .data$beaker_mass_w_OD_sample - .data$beaker_empty_mass - blank_correction,
                    size_class = "clay",
                    mass_pct = .data$clay_mass*40 / .data$OD_specimen_mass) %>%
      dplyr::select(.data$batch_sample_number, .data$size_class, .data$mass_pct)
  ) %>%
    tidyr::drop_na()

  # calculate percent sand
  sand_pcts <-
    datafiles$sieving_data %>%
    dplyr::left_join(OD_specimen_masses) %>%
    dplyr::mutate(size_class = "sand",
                  mass_pct = .data$cumulative_mass_g / .data$OD_specimen_mass ) %>%
    dplyr::select(.data$batch_sample_number, .data$size_class, .data$mass_pct)


  # bind clay and silt tibbles, pivot wider, and calculate silt percents

  size_class_pcts <- rbind(clay_pcts, sand_pcts) %>%
    tidyr::pivot_wider(names_from = .data$size_class,
                       values_from = .data$mass_pct) %>%
    dplyr::mutate(silt = 1 - .data$sand - .data$clay) %>%
    dplyr::select(.data$batch_sample_number, .data$sand, .data$silt, .data$clay) %>%
    dplyr::left_join(datafiles$metadata) %>%
    dplyr::select(.data$date, .data$experiment_name, .data$sample_name, .data$replication, .data$batch_sample_number, .data$sand, .data$silt, .data$clay)


  return(size_class_pcts)

}
