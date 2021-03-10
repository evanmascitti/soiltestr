#' Analyze results of a particle size test with pipette and sieves
#'
#' Uses mass correction for organic matter loss
#'
#' @details This method uses a correction factor based on mass lost during oxidization
#' of organic matter during a pretreatment. The lost mass is converted to an oven-dry
#' % value, which is used the "correct" the original specimen mass.
#'
#' This allows the specimen to be pre-treated to remove OM and then directly advanced to
#' the dispersion step, rather than needing to oven-dry the sample and re-weigh it prior to dispersion.
#'
#' Otherwise, the method is identical to the normal `pipette_analysis()` method.
#'
#' @param dir Directory containing the raw data files
#'
#' @return a list with 3 elements: percent passing data, a breakdown into
#'   traditional size classes, and a list of ggplots (one per specimen)
#' @export
#'
psa.analyze.pipette.OMremoved <- function(dir){

  # read raw data files

  if(stringr::str_sub(string = dir, start = -1) == "/"){
    directory <- dir} else{
      directory <- paste0(dir, "/")
    }

  test_date <- stringr::str_extract(string = directory, pattern = "\\d{4}-\\d{2}\\d{2}")

  paths <- list.files(directory, full.names = T) %>%
    basename(.) %>%
    stringr::str_remove(string = ., pattern = paste0("_", test_date, ".csv"))

  datafiles <- suppressMessages(
    list.files(directory, full.names = T) %>%
      purrr::map(readr::read_csv, na = "-") %>%
      purrr::set_names(paths) %>%
      purrr::map(~janitor::remove_empty(., which = "rows"))
  )

  # change column types to double to account for empty cells containing
  # a dash, which would make them read as character columns

  # make tin tare lookup table from larger data object

  tin_tare_date <-  as.character(unique(datafiles$hygroscopic_correction_data$tin_tare_set))

  tin_tares <- asi468::tin_tares[[tin_tare_date]]


  # make beaker lookup table from larger data object

  beaker_tare_date <- as.character(unique(datafiles$pipetting_data$beaker_tare_set))


  beaker_tares <- asi468::psa_beaker_tares[[beaker_tare_date]]


  # calculate air-dry water contents

  hygroscopic_water_contents <- suppressMessages(
    datafiles$hygroscopic_correction_data %>%
      dplyr::left_join(tin_tares, by = "tin_number")%>%
      soiltestr::add_w() %>%
      dplyr::select(-c(.data$tin_tare_set,
                       .data$tin_number,
                       .data$tin_w_wet_sample,
                       .data$tin_w_OD_sample))
  )


  # compute oven-dry specimen masses WITH OM CORRECTION



  # first calculate loss from OM pretreatment

  OM_pct_losses <- datafiles$OM_loss_correction %>%
    dplyr::left_join(hygroscopic_water_contents, by = c("date", "experiment_name", "sample_name", "batch_sample_number")) %>%
    dplyr:mutate(OD_mass_for_OM_correction = air_dry_specimen_mass_before_treatment / (1 + water_content),
                 OM_mass_loss = container_w_OD_specimen_mass_after_treatment -
                   (OD_mass_for_OM_correction + container_tare),
                 OM_pct_loss = OM_mass_loss / OD_mass_for_OM_correction) %>%
    dplyr::select(.data$experiment_name, .data$date, .data$sample_name, .data$replication,
                  .data$OM_pct_loss)


  OD_specimen_masses <- suppressMessages(
    datafiles$specimen_masses %>%
      dplyr::left_join(hygroscopic_water_contents, by = c("date", "experiment_name", "sample_name", "batch_sample_number")) %>%
      dplyr::left_join(OM_pct_losses, by = c("date", "experiment_name", "sample_name", "batch_sample_number")) %>%
      dplyr::mutate(OD_specimen_mass = (1 - .data$OM_pct_loss) * .data$air_dry_specimen_mass_for_test/(1+.data$water_content)) %>%
      dplyr::select(-c(.data$air_dry_specimen_mass_for_test,
                       .data$tin_tare,
                       .data$water_content))
  )





  # compute blank correction

  blank_correction <- suppressMessages(
    datafiles$blank_correction_data%>%
      dplyr::left_join(beaker_tares, by = "beaker_number") %>%
      dplyr::mutate(calgon_in_beaker = .data$beaker_mass_w_OD_sample - .data$beaker_empty_mass) %>%
      dplyr::summarize(blank_correction = mean(.data$calgon_in_beaker)) %>%
      purrr::pluck(1)
  )


  # calculate soil solids for each beaker and convert to percent finer than each size

  pipette_pct_passing <- suppressMessages(
    datafiles$pipetting_data %>%
      dplyr::left_join(beaker_tares, by = "beaker_number") %>%
      dplyr::left_join(OD_specimen_masses, by = c("date", "experiment_name", "sample_name", "batch_sample_number")) %>%
      dplyr::mutate(soil_solids_in_beaker = .data$beaker_mass_w_OD_sample - .data$beaker_empty_mass - blank_correction,
                    percent_passing = .data$soil_solids_in_beaker*40 / .data$OD_specimen_mass) %>%
      dplyr::select(.data$batch_sample_number, .data$microns, .data$percent_passing)
  )

  # calculate percent finer than each size for the sieve data

  sieve_pct_passing <- suppressMessages(
    datafiles$sieving_data %>%
      dplyr::left_join(OD_specimen_masses, by = c("date", "experiment_name", "sample_name", "batch_sample_number"))%>%
      dplyr::mutate(cumulative_mass_finer = .data$OD_specimen_mass - .data$cumulative_mass_g,
                    percent_passing = .data$cumulative_mass_finer / .data$OD_specimen_mass) %>%
      dplyr::select(.data$batch_sample_number, .data$microns, .data$percent_passing)
  )

  # bind pipette and sieve data together to get a cumulative percent passing tibble

  total_percent_passing <- suppressMessages(
    rbind(pipette_pct_passing, sieve_pct_passing) %>%
      dplyr::arrange(.data$batch_sample_number, dplyr::desc(.data$microns)) %>%
      dplyr::left_join(datafiles$metadata, by = c("date", "experiment_name", "sample_name", "batch_sample_number")) %>%
      dplyr::select(.data$date,
                    .data$experiment_name,
                    .data$sample_name,
                    .data$replication,
                    .data$batch_sample_number,
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
      dplyr::left_join(datafiles$metadata, by = c("date", "experiment_name", "sample_name", "batch_sample_number")) %>%
      dplyr::select(.data$date,
                    .data$experiment_name,
                    .data$sample_name,
                    .data$replication,
                    .data$batch_sample_number,
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
      dplyr::group_by(.data$batch_sample_number) %>%
      tidyr::nest() %>%
      dplyr::rename(psd_tibble = .data$data) %>%
      dplyr::mutate(psd_plot = purrr::map(
        .x = .data$psd_tibble, .f= ~soiltestr::ggpsd_single_sample(df = .) ) ) %>%
      dplyr::select(-.data$psd_tibble) %>%
      dplyr::ungroup() %>%
      dplyr::left_join(datafiles$metadata, by = c("date", "experiment_name", "sample_name", "batch_sample_number")) %>%
      dplyr::select(.data$date,
                    .data$experiment_name,
                    .data$sample_name,
                    .data$replication,
                    .data$batch_sample_number,
                    .data$psd_plot)
  )


  method_metadata <- list(
    measurement_method = c("pipette"),
pretreatments_performed = c("OM removal via 30% hydrogen peroxide"),
dispersion_method = list(
  physical_agitation = "Orbital_mixer_5_min",
  chemical_dispersant = "Sodium hexametaphosphate 5 g/L"
),
references = list(
  "asi468 vignettes",
  "Thurman, Nelson C. Ciolkosz, Edward
Dobos, Robert R., 1994. Penn State Soil Characterization Lab Methods Manual",
"Robinson, 1922"
)
  )

  additional_data <- list(
    OM_pct_losses = OM_pct_losses
  )


    # build a list to return

  # the first three items contain the whole distribution, the size classes, and
  # a plot for each specimen

  # the fourth item is a list with metadata about how the sample was tested

  # the fifth item is a list containing arbitrary length and structure.....
  # in this case it contains just one named data frame with the OM losses.
  # this is easily extensible to include additional data


  psa_results <- list(
    percent_passing = total_percent_passing,
    size_bins = size_bins,
    cumulative_curves = psd_plots,
    method_metadata = method_metadata,
    additional_data = additional_data
)

  return(psa_results)

}
