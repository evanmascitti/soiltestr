#' Analyze the results of a particle size test
#'
#' Accepts a variety of laboratory protocols, see [`psa_protocols()`]
#'
#' @param dir Folder containing the pertinent files
#'
#' @return List of length 6 containing:
#'
#' 1. Data frame of cumulative percent passing data (tidy/long format)
#' 2. Data frame with breakdown into traditional SSSA size classes (wide format)
#' 3. Data frame with breakdown into size sub classes, if measured
#' (wide format); if not possible due to limited number of particle diameters,
#' this element is `NULL`; various breakdowns are available based on the chosen
#' protocol (see [`psa_protocols()`])
#' 4. List of ggplot objects (one per specimen)
#' 5. Metadata about the test protocol
#' 6. If pre-treatment was performed, the loss on pre-treatment for each specimen;
#' otherwise `NULL`
#'
#' @export
#'
psa <- function(dir){

  # determine which protocol was used and assign to a local variable

  protocol_ID <- find_protocol_ID()

  # read all raw data files and put into a list, then divide into common
  # and method-specific lists and clean up any empty rows from each

  test_date <- stringr::str_extract(string = dir, pattern = "\\d{4}-\\d{2}-\\d{2}")

  all_datafile_paths <- divide_psa_datafiles()

# read all the files in; this returns a list of length 2; each element
# is also a list containing the appropriate data frames (tibbles)

datafiles <- purrr::map(all_datafile_paths, import_psa_datafile)

# assign the contents of the datafiles list to the current environment

list2env(datafiles, envir = rlang::current_env())

# for safety, remove the datafiles object since its contents are now dumped
# into the current environment

rm(datafiles)


# compute hygroscopic water contents --------------------------------------

  # first need to pair data with correct set of tin tares

# browser()

  tin_tare_set <- as.character(unique(common_datafiles$hygroscopic_corrections$tin_tare_set))

  tin_tares <- asi468::tin_tares[[tin_tare_set]]


  # calculate air-dry water contents

  hygroscopic_water_contents <- common_datafiles$hygroscopic_corrections %>%
      dplyr::left_join(tin_tares, by = "tin_number")%>%
      soiltestr::add_w() %>%
    dplyr::rename(hygroscopic_water_content = .data$water_content) %>%
    dplyr::select(.data$date:.data$batch_sample_number, .data$hygroscopic_water_content)

  # compute oven-dry specimen masses used in actual PSA tests

  OD_specimen_masses <- common_datafiles$specimen_masses %>%
    dplyr::left_join(hygroscopic_water_contents,
                     by = c("date", "experiment_name", "sample_name",
                            "replication", "batch_sample_number")) %>%
    dplyr::mutate(OD_specimen_mass = .data$air_dry_specimen_mass_for_test / (1 + .data$hygroscopic_water_content)) %>%
  dplyr::select(.data$date:.data$batch_sample_number, .data$OD_specimen_mass)


# determine whether pretreatment correction should be applied
# if it should, apply it; otherwise assign this variable a value of NULL

  use_pretreatment_correction <- check_pretreatment_correction()

  # if it should be, calculate the corrections and then apply them, altering the existing copy of the
  # OD specimen masses data frame


  if(use_pretreatment_correction){

    pretreatment_loss <- compute_pretreatment_loss()

    OD_specimen_masses <- OD_specimen_masses %>%
      dplyr::left_join(
        pretreatment_loss, by = c(
          "date", "experiment_name", "sample_name", "replication",
          "batch_sample_number")) %>%
      dplyr::mutate(OD_specimen_mass = .data$OD_specimen_mass * (1 - .data$pretreatment_loss_pct))
  } else
    pretreatment_loss <- NULL


  # now that the correct specimen mass is known, compute the fines % passing

  fines_percent_passing <- switch (protocol_ID,
    "1" = compute_pipette_fines_pct_passing(),
    "2" = compute_hydrometer_fines_pct_passing(),
    "3" = compute_pipette_fines_pct_passing(),
    "4" = compute_pipette_fines_pct_passing(),
    "5" = compute_hydrometer_fines_pct_passing(),
    "6" = compute_pipette_fines_pct_passing(),
    "7" = compute_pipette_fines_pct_passing(),
    "8" = compute_hydrometer_fines_pct_passing(),
    stop("Can't find the protocol - unable to compute % fines", protocol_ID, call. = T)
  )

  # browser()
# next compute the coarse particles % passing

  coarse_percent_passing <- switch (protocol_ID,
    "1" = compute_sieves_percent_passing(),
    "2" = compute_sieves_percent_passing(),
    "3" = compute_sieves_percent_passing(),
    "4" = compute_sieves_percent_passing(),
    "5" = compute_sieves_percent_passing(),
    "6" = compute_sieves_percent_passing(),
    "7" = compute_sieves_percent_passing(),
    "8" = compute_sieves_percent_passing(),
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

  # create data frame of standard bin sizes

 simple_bins <- simple_bins()



 # check if the protocol permits more complex bin sizes to be computed for
 # the fines method
 # if it does, create a new tibble with the fine-grained sub bins


 if(check_for_fines_complex_bins()){

   # shouldn't need this to ever be called because the logic doesn't allow
   # the protocols to opt into this switch statement but putting in just
   # in case; by assigning as a function I can reduce  duplication

   insufficient_fines_sampling <- function() {
     stop("Only total clay can be computed for this protocol (not enough samples collected).",
          call. = F)
   }

   fines_sub_bins <- switch (protocol_ID,
  "1" = SSSA_pipette_bins(),
  "2" = insufficient_fines_sampling(),
  "3" = SSSA_pipette_bins(),
  "4" = insufficient_fines_sampling(),
  "5" = insufficient_fines_sampling(),
  "6" = SSSA_pipette_bins(),
  "7" = insufficient_fines_sampling(),
  "8" = insufficient_fines_sampling(),
  stop("Could not find any info for psa_protocol ID ", protocol_ID, call. = T)
)


  }




 # check if the protocol permits more complex bin sizes to be computed for
 # the coarse method
 # if it does, create a new tibble with the coarse-grained sub bins

 if(check_for_coarse_complex_bins()){

   # as for the fines, this function should never be called due to
   # logic above but putting here to help with debugging in case
   # something was coded wrong

   insufficient_coarse_sampling <- function() {
     stop("Only total sand and gravel can be computed for this protocol (not enough samples collected)",
          call. = F)
   }

   # browser()

   coarse_sub_bins <- switch(
     protocol_ID,
     "1" = USGA_bins(),
     "2" = USGA_bins(),
     "3" = USGA_bins(),
     "4" = USGA_bins(),
     "5" = USGA_bins(),
     "6" = USGA_bins(),
     "7" = insufficient_coarse_sampling(),
     "8" = insufficient_coarse_sampling(),
     stop("Could not find any info for psa_protocol ID ", protocol_ID, call. = T)
   )

   }

 # browser()

sub_bins <- mget(ls(pattern = "sub_bins")) %>%
  purrr::reduce(
    dplyr::left_join,
    by = c("date", "experiment_name", "protocol_ID", "sample_name",
           "replication", "batch_sample_number")) %>%
  dplyr::arrange(.data$batch_sample_number,
                   .data$replication)


 # make the ggplots for each sample and replication

# don't know where the code to do this went ??

# setting to NULL for now so  I can test other parts of the function
  psd_plots <- NULL

  # psd_plots <- psd_plots_tibble$psd_plot %>%
  #   purrr::set_names(psd_plots_tibble$plot_name)


  # make an extra list containing info about the method and
  # any other data derived from the test (such as loss on pretreatment)

  # obviously this should be chosen with a call to `switch()` but leaving as-is
  # for now

  #browser()

  method_metadata <-switch (protocol_ID,
    "1" = psa_protocols[["1"]],
    # "2" = psa_protocols[["2"]],
     "3" = psa_protocols[["3"]],
    # "4" = psa_protocols[["4"]],
   #  "5" = psa_protocols[["5"]],
    stop("Could not find any info for psa_protocol number", protocol_ID, call. = T))


  # construct list to return
  # though I have used in mget many places
  # I will just explicitly construct this one for
  # extra safety


  # create a variable to append the list for pretreatment;
  # if it was not performed this value is NULL


psa <- mget(
    c("cumulative_percent_passing",
    "simple_bins",
    "sub_bins",
    "psd_plots",
    "method_metadata",
    "pretreatment_loss"))



  return(psa)

  }
