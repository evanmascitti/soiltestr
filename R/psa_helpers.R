
#' Determine which functions to use for analyzing the data set
#'
#' An internal helper function for `psa()`
#'
#' @return Numeric vector of length 1
#'
find_protocol_ID <- function(){

dir <- get(x = "dir", envir = rlang::caller_env())

  # note that in this function the protocol is being read as a character column
  # to facilitate easy use of `switch()` later on

  metadata_file <- readr::read_csv(list.files(path = dir, pattern = "metadata", full.names = T),
                                   col_types = "Dccciic")

  protocol_ID <- as.character(unique(metadata_file$protocol_ID))

  }




#' Divide file paths into common and method-specific
#'
#' Helper function for [`psa()`]. Returns a list of two character vectors - one
#' for the files common to all psa_protocols and one which has
#' the files unique to the protocol in use for this test
#'
#' @return List of length 2
#'
divide_psa_datafiles <- function(){

  #  browser()

  # inherit the dir argument from parent call
  dir <- get(x = "dir", envir = rlang::caller_env())

  all_datafile_paths <-list.files(
    path = dir,
    pattern = "^psa-.*\\d{4}-\\d{2}-\\d{2}\\.csv$",
    full.names = T)

  datafile_names <- stringr::str_remove(
    string = basename(all_datafile_paths),
    pattern = "_\\d{4}-\\d{2}-\\d{2}\\.csv$") %>%
    stringr::str_remove_all("psa-")

  purrr::set_names(all_datafile_paths, datafile_names)

  # find any file paths which match any of the patterns
  # this approach is not very elegant - it could definitely be done with
  # []. %in%, and/or grep or grepl.... however this works and I
  # can't waste any more time on it

  common_patterns <- tibble::tibble(
    pattern1 = "hygroscopic-corrections",
    pattern2 = "metadata",
    pattern3 = "specimen-masses",
    all_datafile_paths = all_datafile_paths
  )


  common_datafiles <- common_patterns %>%
    dplyr::filter(
      stringr::str_detect(all_datafile_paths, pattern = pattern1) |
        stringr::str_detect(all_datafile_paths, pattern = pattern2) |
        stringr::str_detect(all_datafile_paths, pattern = pattern3))%>%
    purrr::pluck("all_datafile_paths")

  method_specific_datafiles <- all_datafile_paths[!all_datafile_paths %in% common_datafiles]

  return_list <- mget(ls(pattern = "_datafiles"), envir = rlang::current_env())

  return(return_list)

}


#' Clean up a vector of file paths and then import each one as a tibble
#'
#' Assign the name while also making sure the protocol ID is imported
#' as a character type
#'
#' @param x character vector of file paths
#'
#' @return list of data frames
#'
import_psa_datafile <- function(x){

  nm <- stringr::str_remove(basename(x), pattern = c("\\.csv$")) %>%
    stringr::str_remove(pattern = c("^psa-") ) %>%
    stringr::str_remove(pattern = c("_\\d{4}-\\d{2}-\\d{2}") ) %>%
    stringr::str_remove(pattern = c("-data$") ) %>%
    stringr::str_replace_all("-", "_")

  # this is a little complex and maybe I could have found a better way to do
  # it, but I finally succeeded in using modify_if.....for each list element
  # (in this particular case there are only 2 but it would generalize),
  # this checks if the data frame has a column named "protocol_ID" and then
  # if it does it applies my anaonymous function to mutate that column
  # into a charcter type. In this way the rest of the data frames are "shielded"
  # from that function; i.e. they will never see it which allows me to use the
  # normal NSE syntax for the mutate call

  # This is a very useful technique and one I should return to for other similar
  # problems.
  # For example, I use the same method to easily modify any data frames that contain
  # the tin tare set or beaker tare set as a character type
  # to have a character type, because this will otherwise mess up joining
  # later in the process. Yea, this is DOPE!

  x %>%
    purrr::set_names(nm) %>%
    purrr::map(readr::read_csv,
               col_types = readr::cols(),
               na = "-",
               trim_ws = TRUE,
               skip_empty_rows = TRUE) %>%
    purrr::modify_if(.p = ~any(names(.) %in% "protocol_ID"),
                     .f = ~dplyr::mutate(. , protocol_ID = as.character(protocol_ID))) %>%
    purrr::modify_if(.p = ~any(names(.) %in% "beaker_tare_set"),
                     .f = ~dplyr::mutate(. , beaker_tare_set = as.character(beaker_tare_set))) %>%
    purrr::modify_if(.p = ~any(names(.) %in% "tin_tare_set"),
                     .f = ~dplyr::mutate(. , tin_tare_set = as.character(tin_tare_set)))


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
#' @return tibble of sample info plus % of OD sample mass lost
#'
compute_pretreatment_loss <- function(){

# assign the required objects to the current function environment from the parent caller

  required_objs <- mget(x = c("dir", "hygroscopic_water_contents", "method_specific_datafiles"),
       envir = rlang::caller_env())

  list2env(required_objs, envir = rlang::current_env())

  # browser()

pretreatment_loss_pcts <- method_specific_datafiles$pretreatment_loss %>%
  dplyr::left_join(hygroscopic_water_contents,
                   by = c("date", "experiment_name", "sample_name", "replication", "batch_sample_number")) %>%
    dplyr::mutate(OD_soil_mass_before_pretreatment = .data$air_dry_specimen_mass_before_pretreatment / (1 + .data$hygroscopic_water_content),
           OD_soil_mass_after_pretreatment = .data$container_mass_w_OD_sample - .data$container_tare,
           pretreatment_loss_mass = .data$OD_soil_mass_before_pretreatment - .data$OD_soil_mass_after_pretreatment,
           pretreatment_loss_pct = .data$pretreatment_loss_mass / .data$OD_soil_mass_before_pretreatment) %>%
    dplyr::select(.data$date:.data$batch_sample_number, .data$pretreatment_loss_pct)

  return(pretreatment_loss_pcts)
}



#' Checks protocol list to see if OD specimen mass must be adjusted
#'
#' @return Logical value of length 1
#'
check_pretreatment_correction <- function(){

  protocol_ID <- get(x = "protocol_ID", envir = rlang::caller_env())

  # return a logical based on a match with the internal data object containing
  # the methods that use pretreatment

  protocol_ID %in% internal_data$pretreatment_invoking_protocol_IDs

  }




#' Calculate % finer than an arbitrary number of pipette sizes
#'
#' Computes blank correction, subtracts from each beaker, then divides beaker
#' contents by overall OD specimen mass
#'
#' @return data frame named `fines_pct_passing`
#'
compute_pipette_fines_pct_passing <- function(){

# inherit the datafiles and OD_specimen masses from the parent function environment



needed_objs <- mget(x = c("method_specific_datafiles", "OD_specimen_masses"),
                    envir = rlang::caller_env())

# make them available in the current function call
list2env(needed_objs,envir = rlang::current_env())

# locate beaker tare set from data files

  beaker_tare_set <- unique(method_specific_datafiles$pipetting$beaker_tare_set)

  beaker_tares <- asi468::psa_beaker_tares[[beaker_tare_set]]

  # compute blank correction

  # browser()

blanks_df <- method_specific_datafiles$pipette_blank_correction %>%
    dplyr::left_join(beaker_tares, by = c("beaker_number", "beaker_tare_set")) %>%
    dplyr::mutate(calgon_in_beaker = .data$beaker_mass_w_OD_sample - .data$beaker_empty_mass)

blank_correction <- mean(blanks_df$calgon_in_beaker, na.rm = TRUE)

  # calculate % passing for each size

# browser()
  fines_percent_passing <- method_specific_datafiles$pipetting %>%
    dplyr::left_join(
      beaker_tares,
      by = c("beaker_number", "beaker_tare_set")) %>%
    dplyr::left_join(
      OD_specimen_masses,
      by = c("date", "experiment_name",
             "sample_name", "replication", "batch_sample_number")) %>%
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
#' @return
#' @export
#'
compute_sieves_percent_passing <- function(){

  # find required objects from calling environment


  needed_objs <- mget(x = c("method_specific_datafiles", "OD_specimen_masses"),
                      envir = rlang::caller_env())

  # make them available in the current function call
  list2env(needed_objs,envir = rlang::current_env())


    sieves_percent_passing <- method_specific_datafiles$sieving %>%
      dplyr::left_join(
        OD_specimen_masses,
        by = c("date", "experiment_name", "sample_name", "replication", "batch_sample_number"))%>%
      dplyr::mutate(cumulative_mass_finer = .data$OD_specimen_mass - .data$cumulative_mass_g,
                    percent_passing = .data$cumulative_mass_finer / .data$OD_specimen_mass) %>%
    dplyr::select(.data$date:.data$batch_sample_number, .data$microns, percent_passing)

    return(sieves_percent_passing)
  }
