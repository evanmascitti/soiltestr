#' Determine which functions to use for analyzing the data set
#'
#' An internal helper function for `psa()`
#'
#' @param dir directory containing the data files
#'
#' @return Numeric vector of length 1
#'
find_protocol_ID <- function(dir){

  # note that in this function the protocol is being read as a character column
  # to facilitate easy use of `switch()` later on

  metadata_file <- readr::read_csv(file = list.files(path = dir, pattern = "metadata", full.names = T),
                                   col_types = readr::cols(
                                     protocol_ID = readr::col_character(),
                                     sample_name = readr::col_character(),
                                     experiment_name = readr::col_character()
                                   ))

  protocol_ID <- unique(metadata_file$protocol_ID)

}

clean_psa_file_name <- function(x){
  basename(x) %>%
    stringr::str_remove(string = ., '_\\d{4}-\\d{2}-\\d{2}[.]csv$') %>%
    stringr::str_remove(., 'psa[_-]') %>%
    stringr::str_replace_all(pattern = "-", replacement = "_")
}





#' Determine loss of mass during specimen pretreatment
#'
#' Rather than oven-drying the pretreated sample and then re-dispersing it,
#' it is often more convenient to run a separate (duplicate) analysis of the
#' pretreatment for organic matter, carbonates, or Fe-oxides and subtract the
#' pretreatement loss from the actual tested specimen's initial oven-dry mass.
#' `compute_preatreatment_loss()` does this by reading a data file titled
#' 'pretreatment_loss_data_\<date\>.csv` and referring to the other files containing
#' the air-dry specimen masses and the hygrscopic water contents.
#'
#' @param dir directory containing the data files
#' @return tibble of sample info plus % of OD sample mass lost
#'
compute_pretreatment_losses <-
  function(dir, hygroscopic_water_contents, method_specific_datafiles) {

    # the bottleneck is in this function..... it is not joining correctly....
    # also seems not to be recognizing the method specific datafiles object ??

    # hygroscopic_water_contents <- hygroscopic_water_contents

    # method_specific_datafiles <- method_specific_datafiles

    # instead of typing all the column names out in quotes, use
    # subsetting to define the vector by which to join

    names_match <- names(method_specific_datafiles$pretreatment_loss_data)[names(method_specific_datafiles$pretreatment_loss_data) %in% names(hygroscopic_water_contents)]

    pretreatment_loss_data <-
      method_specific_datafiles$pretreatment_loss_data %>%
      dplyr::left_join(
        y = hygroscopic_water_contents, by = names_match)



    pretreatment_loss_pcts <- pretreatment_loss_data %>%
      dplyr::mutate(
        OD_soil_mass_before_pretreatment = .data$air_dry_specimen_mass_before_pretreatment / (1 + .data$hygroscopic_water_content),
        OD_soil_mass_after_pretreatment = .data$container_w_OD_specimen_mass_after_pretreatment - .data$container_tare,
        pretreatment_loss_mass = .data$OD_soil_mass_before_pretreatment - .data$OD_soil_mass_after_pretreatment,
        pretreatment_loss_pct = .data$pretreatment_loss_mass / .data$OD_soil_mass_before_pretreatment
      )
    # %>%
    #   dplyr::select(.data$date:.data$batch_sample_number,
    #                 .data$pretreatment_loss_pct)

    return(pretreatment_loss_pcts)
  }



#' Checks protocol list to see if OD specimen mass must be adjusted
#'
#' @param protocol_ID the numbered PSA protocol specified in the list of methods
#'
#' @return Logical value of length 1
#'
check_pretreatment_correction <- function(protocol_ID){


  dplyr::if_else(protocol_ID %in% internal_data$pretreatment_invoking_protocol_IDs,
                 TRUE,
                 FALSE)
}




#' Calculate % finer than an arbitrary number of pipette sizes
#'
#' Computes blank correction, subtracts from each beaker, then divides beaker
#' contents by overall OD specimen mass
#'
#' @return data frame named `fines_pct_passing`
#'
compute_pipette_fines_pct_passing <- function(method_specific_datafiles, OD_specimen_masses){



  # locate beaker tare set from data file

  beaker_tare_set <- unique(
    as.character(method_specific_datafiles$pipetting_data$beaker_tare_set))

  # compute blank correction

  blanks_df <- method_specific_datafiles$blank_correction_data %>%
    dplyr::left_join(asi468::psa_beaker_tares[[beaker_tare_set]], by = "beaker_number") %>%
    dplyr::mutate(calgon_in_beaker = .data$beaker_mass_w_OD_sample - .data$beaker_empty_mass)

  blank_correction <- mean(blanks_df$calgon_in_beaker, na.rm = TRUE)

  # calculate % passing for each size

  fines_percent_passing <- method_specific_datafiles$pipetting_data %>%
    dplyr::left_join(asi468::psa_beaker_tares[[beaker_tare_set]], by = "beaker_number") %>%
    dplyr::left_join(OD_specimen_masses, by = c("date", "experiment_name", "sample_name", "replication", "batch_sample_number")) %>%
    dplyr::mutate(total_g_in_beaker = .data$beaker_mass_w_OD_sample - .data$beaker_empty_mass,
                  soil_in_beaker = .data$total_g_in_beaker - blank_correction,
                  percent_passing = 40 * .data$soil_in_beaker / .data$OD_specimen_mass) %>%
    dplyr::select(.data$date:.data$batch_sample_number, .data$microns, percent_passing)

  return(fines_percent_passing)
}


# will obviously need to come back to this one; it should also return a data frame
# called `fines_percent_passing()` with an arbitrary number of particle diameters:


# compute_hydrometer_fines_pct_passing <- function(){
#
# }



#' Calculate % finer for arbitrary number of sieves
#'
#' @param method_specific_datafiles List of input data; constructed by initial call to [`psa()`]
#' @param OD_specimen_masses Data frame of oven-dry specimen masses; also
#'   contstructed by initial call to [`psa()`]
#'
#' @return
#' @export
#'
compute_sieves_percent_passing <- function(method_specific_datafiles, OD_specimen_masses){

  sieves_percent_passing <- method_specific_datafiles$sieving_data %>%
    dplyr::left_join(OD_specimen_masses, by = c("date", "experiment_name", "sample_name", "replication", "batch_sample_number"))%>%
    dplyr::mutate(cumulative_mass_finer = .data$OD_specimen_mass - .data$cumulative_mass_g,
                  percent_passing = .data$cumulative_mass_finer / .data$OD_specimen_mass) %>%
    dplyr::select(.data$date:.data$batch_sample_number, .data$microns, percent_passing)

  return(sieves_percent_passing)
}
