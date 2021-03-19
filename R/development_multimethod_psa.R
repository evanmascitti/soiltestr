#' Analyze the results of a particle size test
#'
#' Accepts a variety of laboratory protocols, see [`psa_protocols()`]
#'
#' @param dir Folder containing the pertinent files
#'
#' @return List of length 4 containing:
#'
#' 1. Data frame of cumulative percent passing data in tidy format
#' 2. Data frame with breakdown into traditional size classes (wide format)
#' 3. List of ggplots (one per specimen)
#' 4. Metadata about the test protocol
#' 5. If pretreatment was performed, the loss on pretreatment for each specimen
#'
#' @export
#'
psa <- function(dir){

  # append dir argumnt if no trailing slash was provided

  if(stringr::str_sub(string = dir, start = -1) == "/"){
    directory <- dir} else{
      directory <- paste0(dir, "/")
    }

  # determine which protocol was used and assign to a local variable

  protocol_number <- find_protocol(dir = directory)

  # read all raw data files and put into a list



  test_date <- stringr::str_extract(string = directory, pattern = "\\d{4}-\\d{2}-\\d{2}")

  # I would prefer to read all the files in with purrr::map() but since the column names
  # and types are all different, I will still keep them inside a list but they will be imported
  # with individual calls to readr::read_csv(). At first only import the files common
  # to all methods, then append the list conditionally if other files are required for the
  # particular method

  datafiles <- list(
    hygroscopic_correction_data = readr::read_csv(file = list.files(
      path = dir, pattern = "hygroscopic_correction_data", full.names = T),
      col_types = "Dcciiciddc", na = c("-", "")),
    metadata= readr::read_csv(file = list.files(
      path = dir, pattern = "metadata", full.names = T),
      col_types = "Dcciiic", na = c("-", "")),
    sieving_data = readr::read_csv(file = list.files(
      path = dir, pattern = "sieving_data", full.names = T),
      col_types = "Dcciiddc", na = c("-", "")),
    specimen_masses_data = readr::read_csv(file = list.files(
      path = dir, pattern = "specimen_masses_data", full.names = T),
      col_types = "Dcciidc", na = c("-", ""))
  )

  # add pipette data and blank correction data if the protocol uses
  # the pipette method

  if(protocol_number %in% as.character(c(1, 3, 4, 6))) {

    datafiles$pipetting_data <- readr::read_csv(file = list.files(
      path = dir, pattern = "pipetting_data", full.names = T),
      col_types = "Dcciiicdidc", na = c("-", ""))

    datafiles$ blank_correction_data <- readr::read_csv(file = list.files(
      path = dir, pattern = "blank_correction_data", full.names = T),
      col_types = "Dciicidc", na = c("-", ""))


  }

  # clean up any empty rows

  datafiles <- purrr::map(datafiles, janitor::remove_empty, which = 'rows')

# compute hygroscopic water contents --------------------------------------

  # first need to pair data with correct set of tin tares

  tin_tare_date <- unique(datafiles$hygroscopic_correction_data$tin_tare_set)

  tin_tares <- asi468::tin_tares[[tin_tare_date]]


  # calculate air-dry water contents

  hygroscopic_water_contents <- datafiles$hygroscopic_correction_data %>%
      dplyr::left_join(tin_tares, by = "tin_number")%>%
      soiltestr::add_w() %>%
    dplyr::rename(hygroscopic_water_content = .data$water_content) %>%
    dplyr::select(.data$date:.data$batch_sample_number, .data$hygroscopic_water_content)


  # compute oven-dry specimen masses used in actual PSA tests

  OD_specimen_masses <- datafiles$specimen_masses %>%
    dplyr::left_join(hygroscopic_water_contents,
                     by = c("date", "experiment_name", "sample_name", "replication", "batch_sample_number")) %>%
    dplyr::mutate(OD_specimen_mass = .data$air_dry_specimen_mass_for_test / (1 + .data$hygroscopic_water_content)) %>%
  dplyr::select(.data$date:.data$batch_sample_number, .data$OD_specimen_mass)


# determine whether pretreatment correction should be applied

  use_pretreatment_correction <- check_pretreatment_correction(protocol = protocol_number)

  # if it should be, calculate the corrections and then apply them, altering the existing copy of the
  # OD specimen masses data frame

  if(use_pretreatment_correction){

    pretreatment_losses <- compute_pretreatment_losses(dir = directory,
                                                       hygroscopic_water_contents = hygroscopic_water_contents)

    OD_specimen_mases <- OD_specimen_masses %>%
      dplyr::left_join(pretreatment_losses, by = c("date", "experiment_name",
                                                   "sample_name", "replication",
                                                   "batch_sample_number")) %>%
      dplyr::mutate(OD_specimen_mass = .data$OD_specimen_mass * (1 - .data$pretreatment_loss_pct))
  }


  # now that the correct specimen mass is known, compute the fines % passing



fines_percent_passing <- switch (protocol_number,
    "1" = compute_pipette_fines_pct_passing(datafiles = datafiles, OD_specimen_masses = OD_specimen_masses),
    "2" = compute_hydrometer_fines_pct_passing(),
    "3" = compute_pipette_fines_pct_passing(datafiles = datafiles, OD_specimen_masses = OD_specimen_masses),
    "4" = compute_pipette_fines_pct_passing(datafiles = datafiles, OD_specimen_masses = OD_specimen_masses),
    "5" = compute_hydrometer_fines_pct_passing(),
    stop("Can't find the protocol - unable to compute % fines", protocol_number, call. = T)
  )

# next compute the coarse particles % passing

  coarse_percent_passing <- switch (protocol_number,
    "1" = compute_sieves_percent_passing(datafiles = datafiles, OD_specimen_masses = OD_specimen_masses),
    "2" = compute_sieves_percent_passing(datafiles = datafiles, OD_specimen_masses = OD_specimen_masses),
    "3" = compute_sieves_percent_passing(datafiles = datafiles, OD_specimen_masses = OD_specimen_masses),
    "4" = compute_sieves_percent_passing(datafiles = datafiles, OD_specimen_masses = OD_specimen_masses),
    "5" = compute_sieves_percent_passing(datafiles = datafiles, OD_specimen_masses = OD_specimen_masses),
    stop(
      "Can't find the protocol - unable to compute % coarse particles",
      protocol_number,
      call. = T
    )
  )

  # bind coarse and fine data frames together

  cumulative_percent_passing <- rbind(fines_percent_passing,
                                      coarse_percent_passing) %>%
    dplyr::arrange(.data$batch_sample_number,
                   dplyr::desc(.data$microns))


  # calculate the percent of the sample in each traditional size class
  # need to come up with some conditional logic to only perform these computations
  # if the particular sizes were measured. Leaving in for now with the understanding
  # that it will thrown an error if the micron values are not found

# UPDATE  see conceptual ideas.md file for an idea

  size_bins <- cumulative_percent_passing %>%
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

  # make a list containing a psd plot for each specimen
  psd_plots_tibble <- cumulative_percent_passing %>%
    dplyr::arrange(.data$batch_sample_number) %>%
    dplyr::group_by(.data$batch_sample_number) %>%
    tidyr::nest() %>%
    dplyr::rename(psd_tibble = .data$data) %>%
    dplyr::mutate(
      psd_plot = purrr::map(
        .x = .data$psd_tibble,
        .f = ~ soiltestr::ggpsd_single_sample(df = .)
      )) %>%
    dplyr::left_join(datafiles$metadata, by = "batch_sample_number") %>%
      dplyr::mutate(plot_name = paste0(.data$sample_name, " | ", "replication ", .data$replication)
      ) %>%
    dplyr::ungroup()

  psd_plots <- psd_plots_tibble$psd_plot %>%
    purrr::set_names(psd_plots_tibble$plot_name)


  # make an extra list containing info about the method and
  # any other data derived from the test (such as loss on pretreatment)

  # obviously this should be chosen with a call to `switch()` but leaving as-is
  # for now

  #browser()

  method_metadata <-switch (protocol_number,
    "1" = soiltestr::psa_protocols[["1"]],
    # "2" = psa_protocols[["2"]],
     "3" = soiltestr::psa_protocols[["3"]],
    # "4" = psa_protocols[["4"]],
   #  "5" = psa_protocols[["5"]],
    stop("Could not find any info for psa_protocol number", protocol_number, call. = T))


  # construct list to return

  psa <- list(
    cumulative_percent_passing = cumulative_percent_passing,
    size_bins = size_bins,
    psd_plots = psd_plots,
    method_metadata = method_metadata,
    pretreatment_losses = if(use_pretreatment_correction){pretreatment_losses} else{
      NULL
    }
    )

  }
