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
    "9" = compute_hydrometer_fines_pct_passing(),
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
    "9" = compute_sieves_percent_passing(),
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
  "9" = insufficient_fines_sampling(),
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
     "9" = insufficient_coarse_sampling(),
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



# make the ggplots for each sample and replication ---------------------

base_plots <- cumulative_percent_passing %>%
  dplyr::left_join(psa_protocols_summary, by = "protocol_ID") %>%
  dplyr::group_by(.data$batch_sample_number) %>%
  tidyr::nest() %>%
  dplyr::mutate(plot = purrr::map(data, ggpsd)) %>%
  purrr::pluck("plot")

# this is a hack to filter to a single row per replication
# not ideal but it works

plots_extra_stuff <- cumulative_percent_passing %>%
  dplyr::group_by(dplyr::across(.cols = .data$date:.data$batch_sample_number)) %>%
  dplyr::summarise() %>%
  dplyr::left_join(psa_protocols_summary, by = "protocol_ID") %>%
  dplyr::mutate(subtitle = paste(
  " Sample name: ", .data$sample_name, "\n",
  "Replication ", .data$replication, "\n") )

# this was some extra stuff I was going to include right on the plot
# but I think it clutters it too much
# this is mostly for EDA/qc anwyay; for a publication or
# polished report I would do this in R Markdown

  # "Fines sampling: ", .data$fines_method, "\n",
#  g_sample, " g sample", "\n",
#  "[See protocol ", protocol_ID, "for other method details.") ) %>%

plot_subtitles <- plots_extra_stuff$subtitle

plot_names <- paste0(plots_extra_stuff$sample_name, "_rep", plots_extra_stuff$replication)

add_subtitle <- function(plot, text) {
  plot +
    labs(subtitle = text)+
    ggplot2::theme(plot.subtitle = ggplot2::element_text(size = 8))
}


psd_plots <- purrr::map2(base_plots, plot_subtitles, add_subtitle) %>%
  purrr::set_names(plot_names)


################################################################################

browser()

method_metadata <-switch (protocol_ID,
    "1" = psa_protocols[["1"]],
    "2" = psa_protocols[["2"]],
    "3" = psa_protocols[["3"]],
    "4" = psa_protocols[["4"]],
    "5" = psa_protocols[["5"]],
    "6" = psa_protocols[["6"]],
    "7" = psa_protocols[["7"]],
    "8" = psa_protocols[["8"]],
    "9" = psa_protocols[["9"]],
    stop("Could not find any info for psa_protocol number", protocol_ID, call. = T))


  # make another list that has the averages of each variable for the applicable
  # data frames produced above

  # this uses some helpers defined in psa-summarizing-helpers.R

  # browser()

  averages <- mget(x = c("cumulative_percent_passing", "simple_bins", "sub_bins", "pretreatment_loss"),
              envir = rlang::current_env()) %>%
    purrr::modify_if(
      .p = ~ "microns" %in% names(.),
      .f = pivot_cumulative_percent_passing_wider) %>%
    purrr::map(summarize_psa) %>%
    purrr::modify_if(
      .p = ~ any(stringr::str_detect(string = names(.), pattern = "^\\d")),
      .f = pivot_cumulative_percent_passing_longer)

  # construct list to return

psa <- mget(
    c("cumulative_percent_passing",
    "simple_bins",
    "sub_bins",
    "method_metadata",
    "pretreatment_loss",
    "averages",
    "psd_plots"))



  return(psa)

  }
