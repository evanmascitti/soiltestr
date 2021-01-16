#' @title Generate an empty data sheet
#'
#' @description Populates several empty csv files with the correct column names
#'   and structure. Use for pipette method of particle size analysis.
#'
#' @param date date test was **begun** by weighing air-dry specimens, yyyy-mm-dd
#'   format
#' @param experiment_name experiment with which the samples are associated
#' @param sample_IDs a character vector of unique identifiers for the samples
#'   tested (for example, "EPK kaolin")
#' @param dir directory to write the skeleton file (with trailing slash)
#' @param n_reps number of replicate specimens tested per sample
#' @param n_pipette_sizes the number of pipette samples withdrawn; typically 4
#'   (sizes are 53, 20, 2, and 0.2 microns)
#'
#' @details The date refers to the date the first step of the test was begun. As
#'   most soil tests span multiple days, this convention avoids any ambiguity
#'   about when they were weighed, tested, etc.
#'
#' @return file written to disk and a message is printed
#' @export
#'

new_pipette_datasheets <- function(date, experiment_name, sample_IDs, dir, n_reps = 1, n_pipette_sizes = 4){


  skeleton_psa_metadatasheet <- tibble::tibble(
    date = date,
    experiment_name = experiment_name,
    sample_number = rep(1:length(sample_IDs), each = n_reps),
    sample_ID = rep(sample_IDs, each = n_reps),
    comments = "-"
  )

  skeleton_psa_specimen_masses_datasheet <- tibble::tibble(
    date = date,
    experiment_name = experiment_name,
    sample_number = rep(1:length(sample_IDs), each = n_reps),
    sample_ID = rep(sample_IDs, each = n_reps),
    air_dry_specimen_mass_for_test = "",
    hygroscopic_correction_tin_number = "",
    tin_w_wet_sample = "",
    tin_w_OD_sample = "",
    comments = "-"
  )

  skeleton_psa_pipetting_datasheet <- tibble::tibble(
    date = date,
    experiment_name = experiment_name,
    sample_number = rep(1:length(sample_IDs), each = n_reps),
    sample_ID = rep(sample_IDs, each = n_reps),
    beaker_tare_set = "",
    beaker_number = "",
    beaker_OD_mass_w_sample = "",
    comments = "-"
  )

  skeleton_blank_correction_pipetting_datasheet <- tibble::tibble(
    date = date,
    experiment_name = experiment_name,
    blank_rep = 1:n_reps,
    beaker_tare_set = "",
    beaker_number = "",
    beaker_OD_mass_w_sample = "",
    comments = "-"
  )

  all_datasheets <- list(
    psa_metadata = skeleton_psa_metadatasheet,
    psa_specimen_masses = skeleton_psa_specimen_masses_datasheet,
    psa_pipetting_data = skeleton_psa_pipetting_datasheet,
    blank_correction = skeleton_blank_correction_pipetting_datasheet
  )

  files_to_write <- all_datasheets %>%
    tibble::enframe(value = "x") %>%
    dplyr::select(-name) %>%
    dplyr::mutate(file = paste0(dir, "/", experiment_name, "_", date, "_", names(all_datasheets), ".csv")
    )

  purrr::pwalk(.l = files_to_write, .f = readr::write_csv)

  message(crayon::green("Please verify that files were written to disk."))
}



