#' Analyze the results of a particle size test
#'
#' Accepts a variety of laboratory protocols, see [`psa_protocols()`]
#'
#' @param dir Folder containing the pertinent files
#'
#' @return List of length 5 containing:
#'
#' 1. Data frame of cumulative percent passing data (tidy/long format)
#' 2. Data frame with breakdown into traditional size classes (wide format)
#' 3. List of ggplot objects (one per specimen)
#' 4. Metadata about the test protocol
#' 5. If pretreatment was performed, the loss on pre-treatment for each specimen;
#' otherwise `NULL`
#'
#' @export
#'
psa <- function(dir){



  # append dir argument if no trailing slash was provided

  if(stringr::str_sub(string = dir, start = -1) == "/"){
    directory <- dir} else{
      directory <- paste0(dir, "/")
    }

  # determine which protocol was used and assign to a local variable

  protocol_ID <- find_protocol_ID(dir = directory)

  # read all raw data files and put into a list
  # use a safe regex to pull out the date from the directory name


  test_date <- stringr::str_extract(string = directory, pattern = "(?<=psa-data_)\\d{4}-\\d{2}-\\d{2}")

  # construct named character vectors containing all paths to relevant PSA files in  the directory - use a helper function to keep this function cleaner

  datafile_paths <- divide_psa_datafiles(directory = directory)

  common_datafile_paths <- datafile_paths$common_file_paths %>%
    purrr::set_names(nm = clean_psa_file_name(.))

  method_specific_datafile_paths <- datafile_paths$method_specific_datafile_paths %>%
    purrr::set_names(nm = clean_psa_file_name(.))

  rm(datafile_paths)


  # read data into R

  common_datafiles <- purrr::map(common_datafile_paths,
                                 readr::read_csv,
                                 na = c("-", ""),
                                 col_types = readr::cols(
                                   protocol_ID = readr::col_character(),
                                   sample_name = readr::col_character()
                                 ))
  # %>%
  #   purrr::map(janitor::remove_empty, which = 'rows')
  #
  # %>%
  #   purrr::set_names(names(common_datafile_paths))

  method_specific_datafiles <- purrr::map(method_specific_datafile_paths,
                                          readr::read_csv,
                                          na = c("-", ""),
                                          col_types = readr::cols(
                                            protocol_ID = readr::col_character()
                                            #sample_name = readr::col_character()
                                          )) %>%
    purrr::map(janitor::remove_empty, which = 'rows')



  # compute values whose process is common to all methods  -------------------


  # compute hygroscopic water contents --------------------------------------

  # first need to pair data with correct set of tin tares




  tin_tare_date <- as.character(unique(common_datafiles$hygroscopic_corrections$tin_tare_set))

  tin_tares <- asi468::tin_tares[[tin_tare_date]]


  # calculate air-dry water contents

  hygroscopic_water_contents <- common_datafiles$hygroscopic_corrections %>%
    dplyr::left_join(tin_tares, by = "tin_number")%>%
    soiltestr::add_w() %>%
    dplyr::rename(hygroscopic_water_content = .data$water_content) %>%
    dplyr::select(.data$date:.data$batch_sample_number, .data$hygroscopic_water_content)


  # compute oven-dry specimen masses used in actual PSA tests

  OD_specimen_masses <- common_datafiles$specimen_masses %>%
    dplyr::left_join(hygroscopic_water_contents,
                     by = c("date", "experiment_name", "sample_name", "replication", "batch_sample_number")) %>%
    dplyr::mutate(OD_specimen_mass = .data$air_dry_specimen_mass_for_test / (1 + .data$hygroscopic_water_content)) %>%
    dplyr::select(.data$date:.data$batch_sample_number, .data$OD_specimen_mass)



  # determine whether pretreatment correction should be applied

  use_pretreatment_correction <- check_pretreatment_correction(protocol_ID = protocol_ID)

  # if it should be, calculate the corrections and then apply them, altering the existing copy of the
  # OD specimen masses data frame



  if(use_pretreatment_correction){

    pretreatment_losses <- compute_pretreatment_losses(
      dir = directory,
      hygroscopic_water_contents = hygroscopic_water_contents,
      method_specific_datafiles = method_specific_datafiles)

    OD_specimen_mases <- OD_specimen_masses %>%
      dplyr::left_join(pretreatment_losses, by = c("date", "experiment_name",
                                                   "sample_name", "replication",
                                                   "batch_sample_number")) %>%
      dplyr::mutate(OD_specimen_mass = .data$OD_specimen_mass * (1 - .data$pretreatment_loss_pct))
  }

  # now that the correct specimen mass is known, compute the fines % passing


  # There must be a way to select this first from the protocols
  # using indexing, and make the call to `switch` more elegant...
  # can't worry about it right now though


  fines_percent_passing <- switch (protocol_ID,
                                   "1" = compute_pipette_fines_pct_passing(method_specific_datafiles = method_specific_datafiles, OD_specimen_masses = OD_specimen_masses),
                                   "2" = compute_hydrometer_fines_pct_passing(),
                                   "3" = compute_pipette_fines_pct_passing(method_specific_datafiles = method_specific_datafiles, OD_specimen_masses = OD_specimen_masses),
                                   "4" = compute_pipette_fines_pct_passing(method_specific_datafiles = method_specific_datafiles, OD_specimen_masses = OD_specimen_masses),
                                   "5" = compute_hydrometer_fines_pct_passing(),
                                   stop("Can't find the protocol - unable to compute % fines", protocol_ID, call. = T)
  )

  # next compute the coarse particles % passing
  # see comment above re: switch and a more elegant solution
  coarse_percent_passing <- switch (protocol_ID,
                                    "1" = compute_sieves_percent_passing(method_specific_datafiles = method_specific_datafiles, OD_specimen_masses = OD_specimen_masses),
                                    "2" = compute_sieves_percent_passing(method_specific_datafiles = method_specific_datafiles, OD_specimen_masses = OD_specimen_masses),
                                    "3" = compute_sieves_percent_passing(method_specific_datafiles = method_specific_datafiles, OD_specimen_masses = OD_specimen_masses),
                                    "4" = compute_sieves_percent_passing(method_specific_datafiles = method_specific_datafiles, OD_specimen_masses = OD_specimen_masses),
                                    "5" = compute_sieves_percent_passing(method_specific_datafiles = method_specific_datafiles, OD_specimen_masses = OD_specimen_masses),
                                    stop(
                                      "Can't find the protocol - unable to compute % coarse particles",
                                      protocol_ID,
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


  # Here I construct the simple bins for any test,
  # And add another list element which will be computed
  # only if applicable.....otherwise the element gets NULL
  standard_size_bins <- simple_bins()

  detailed_size_bins <- switch (
    protocol_ID,
    "1"  = all_bins(),
    "2"  = NULL,
    "3"  = all_bins(),
    "4"  = NULL,
    "5"  = NULL,
    "6"  = all_bins(),
    "7"  = NULL,
    "8"  = NULL,
  )



  # I am sure this or a similar comparison will eventually be useful......

  # if(!protocol_ID %in% multi_size_pipette_protocol_IDs &&
  #    !protocol_ID %in% basic_coarse_bins_protocol_IDs){

  # either with switch or other conditional logic??


  # browser()

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
    dplyr::left_join(common_datafiles$metadata, by = "batch_sample_number") %>%
    dplyr::mutate(plot_name = paste0(.data$sample_name, " | ", "replication ", .data$replication)
    ) %>%
    dplyr::ungroup()

  psd_plots <- psd_plots_tibble$psd_plot %>%
    purrr::set_names(psd_plots_tibble$plot_name)


  # make an extra list containing info about the method and
  # any other data derived from the test (such as loss on pretreatment)

  # obviously this should be chosen with a call to `switch()` but leaving as-is
  # for now

  method_metadata <- psa_protocols[[protocol_ID]]

  # construct list to return

  psa_return_list <- list(
    cumulative_percent_passing = cumulative_percent_passing,
    standard_size_bins = standard_size_bins,
    detailed_size_bins = detailed_size_bins,
    psd_plots = psd_plots,
    method_metadata = method_metadata,
    pretreatment_losses = if(use_pretreatment_correction){pretreatment_losses} else{
      NULL
    }
  )

  return(psa_return_list)

}
