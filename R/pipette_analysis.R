#' Analyze results of a particle size test with pipette and sieves
#'
#' @param dir Directory containing the raw data files
#'
#' @return a list with 3 elements: percent passing data, a breakdown into
#'   traditional size classes, and a list of ggplots (one per specimen)
#' @export
#'
pipette_analysis <- function(dir){

  # read raw data files

  if(stringr::str_sub(string = dir, start = -1) == "/"){
    directory <- dir} else{
      directory <- paste0(dir, "/")
    }

  test_date <- stringr::str_sub(string = directory, start = -14, end = -5)

  paths <- list.files(directory, full.names = T) %>%
    basename(.) %>%
    stringr::str_remove(string = ., pattern = paste0("_", test_date, ".csv"))

  datafiles <- suppressMessages(
    list.files(directory, full.names = T) %>%
      purrr::map(readr::read_csv) %>%
      purrr::set_names(paths) %>%
      purrr::map(~dplyr::select(., -comments))
  )


  # make tin tare lookup table from larger data object

  tin_tare_date <-  unique(datafiles$hygroscopic_correction_data$tin_tare_set)

  tin_tares <- asi468::tin_tares %>%
    tibble::enframe(name= "date", value = "tin_tares_data") %>%
    tidyr::unnest(.data$tin_tares_data) %>%
    dplyr::filter(date == tin_tare_date) %>%
    dplyr::select(-.data$date)


  # make beaker lookup table from larger data object

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

  OD_specimen_masses <- suppressMessages(
    datafiles$specimen_masses %>%
      dplyr::left_join(hygroscopic_water_contents) %>%
      dplyr::mutate(OD_specimen_mass = .data$air_dry_specimen_mass_for_test/(1+.data$water_content)) %>%
      dplyr::select(-c(.data$air_dry_specimen_mass_for_test,
                       .data$tin_tare,
                       .data$water_content))
  )


  # compute blank correction

  blank_correction <- suppressMessages(
    datafiles$blank_correction_data%>%
      dplyr::left_join(beaker_tares) %>%
      dplyr::mutate(calgon_in_beaker = .data$beaker_mass_w_OD_sample - .data$beaker_empty_mass) %>%
      dplyr::summarize(blank_correction = mean(.data$calgon_in_beaker)) %>%
      purrr::pluck(1)
  )


  # calculate soil solids for each beaker and convert to percent finer than each size

  pipette_pct_passing <- suppressMessages(
    datafiles$pipetting_data %>%
      dplyr::left_join(beaker_tares) %>%
      dplyr::left_join(OD_specimen_masses) %>%
      dplyr::mutate(soil_solids_in_beaker = .data$beaker_mass_w_OD_sample - .data$beaker_empty_mass - blank_correction,
                    percent_passing = .data$soil_solids_in_beaker*40 / .data$OD_specimen_mass) %>%
      dplyr::select(.data$sample_number, .data$microns, .data$percent_passing)
  )

  # calculate percent finer than each size for the sieve data

  sieve_pct_passing <- suppressMessages(
    datafiles$sieving_data %>%
      dplyr::left_join(OD_specimen_masses)%>%
      dplyr::mutate(cumulative_mass_finer = .data$OD_specimen_mass - .data$cumulative_mass_g,
                    percent_passing = .data$cumulative_mass_finer / .data$OD_specimen_mass) %>%
      dplyr::select(.data$sample_number, .data$microns, .data$percent_passing)
  )

  # bind pipette and sieve data together to get a cumulative percent passing tibble

  total_percent_passing <- suppressMessages(
    rbind(pipette_pct_passing, sieve_pct_passing) %>%
      dplyr::arrange(.data$sample_number, dplyr::desc(.data$microns)) %>%
      dplyr::left_join(datafiles$metadata) %>%
      dplyr::select(.data$date,
                    .data$experiment_name,
                    .data$sample_name,
                    .data$replication,
                    .data$sample_number,
                    .data$microns,
                    .data$percent_passing)
  )


  # calculate the percent of the sample in each traditional size class

  size_bins <- suppressMessages(
    total_percent_passing %>%
      tidyr::pivot_wider(names_from = .data$microns, values_from = .data$percent_passing) %>%
      dplyr::mutate(
        gravel = .data$`4000` - .data$`2000`,
        very_coarse_sand = .data$`2000` - .data$`1000`,
        coarse_sand = .data$`1000` - .data$`500`,
        medium_sand = .data$`500` - .data$`250`,
        fine_sand = .data$`250` - .data$`150`,
        very_fine_sand = .data$`150` - .data$`53`,
        coarse_silt = .data$`53` - .data$`20`,
        medium_silt = .data$`20` - .data$`5`,
        fine_silt = .data$`5` - .data$`2`,
        coarse_clay = .data$`2` - .data$`0.2`,
        fine_clay = .data$`0.2`,
        sand = .data$very_coarse_sand + .data$coarse_sand + .data$medium_sand + .data$fine_sand + .data$very_fine_sand,
        silt = .data$coarse_silt + .data$medium_silt + .data$fine_silt,
        clay = .data$coarse_clay + .data$fine_clay) %>%
      dplyr::left_join(datafiles$metadata) %>%
      dplyr::select(.data$date,
                    .data$experiment_name,
                    .data$sample_name,
                    .data$replication,
                    .data$sample_number,
                    .data$gravel,
                    .data$sand,
                    .data$silt,
                    .data$clay,
                    .data$very_coarse_sand:.data$fine_clay) %>%
      dplyr::mutate(dplyr::across(.cols = .data$gravel:.data$fine_clay, .fns = ~.*100))
  )

  # make a list containing a psd plot for each specimen
  psd_plots <- suppressMessages(
    total_percent_passing %>%
      dplyr::group_by(.data$sample_number) %>%
      tidyr::nest() %>%
      dplyr::rename(psd_tibble = .data$data) %>%
      dplyr::mutate(psd_plot = purrr::map(
        .x = .data$psd_tibble, .f= ~soiltestr::ggpsd_single_sample(df = .) ) ) %>%
      dplyr::select(-.data$psd_tibble) %>%
      dplyr::ungroup() %>%
      dplyr::left_join(datafiles$metadata) %>%
      dplyr::select(.data$date,
                    .data$experiment_name,
                    .data$sample_name,
                    .data$replication,
                    .data$sample_number,
                    .data$psd_plot)
  )


  return(
    list(
      percent_passing = total_percent_passing,
      size_bins = size_bins,
      cumulative_curves = psd_plots)
  )

}
