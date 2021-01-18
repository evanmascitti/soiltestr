#' @title Generate an empty data sheet
#'
#' @description Populates several empty csv files with the correct column names
#'   and structure. Use for pipette method of particle size analysis.
#'
#' @param date date test was **begun** by weighing air-dry specimens, yyyy-mm-dd
#'   format
#' @param experiment_name experiment with which the samples are associated
#' @param sample_names a character vector of unique identifiers for the samples
#'   tested (for example, "EPK kaolin")
#' @param dir directory to write the skeleton file (with trailing slash)
#' @param n_reps number of replicate specimens tested per sample
#' @param pipette_sizes numeric vector of particle diameters for which to
#'   sample; typically 20, 5, 2, and 0.2 &mum)
#' @param sieves_um numeric or character vector containing the aperture size of
#'   the sieves to be used (in micrometers)
#'
#' @details The date refers to the date the first step of the test was begun. As
#'   most soil tests span multiple days, this convention avoids any ambiguity
#'   about when they were weighed, tested, etc.
#'
#' @return file written to disk and a message is printed
#' @export
#'

new_pipette_datasheets <- function(date, experiment_name, sample_names,
                                   dir, n_reps = 1, pipette_sizes = c(20, 5, 2, 0.2),
                                   sieves_um = c(2000, 1000, 500, 250, 150, 53)){

  new_folder <- paste0(dir, "pipette_w_sieves_data", "_", date)

  dir.create(new_folder)

  skeleton_psa_metadatasheet <- tibble::tibble(
    date = date,
    experiment_name = experiment_name,
    sample_name = sample_names,
    comments = "-"
  )

  skeleton_psa_specimen_masses_datasheet <- tibble::tibble(
    date = date,
    experiment_name = experiment_name,
    sample_name = rep(sample_names, each = n_reps),
    replication = rep(1:n_reps, times = length(sample_names)),
    sample_number = 1:(length(sample_names)*n_reps),
    air_dry_specimen_mass_for_test = "",
    tin_tare_set = "",
    tin_number = "",
    tin_w_wet_sample = "",
    tin_w_OD_sample = "",
    comments = "-"
  )

  skeleton_psa_pipetting_datasheet <- tibble::tibble(
    date = date,
    experiment_name = experiment_name,
    sample_name = rep(sample_names, each = n_reps*length(pipette_sizes)),
    replication = rep(1:n_reps, times = length(sample_names)*length(pipette_sizes)),
    sample_number = rep(1:(length(sample_names)*n_reps), each = length(pipette_sizes)),
    bouyoucous_cylinder_number = .data$sample_number,
    beaker_tare_set = "",
    microns = rep(pipette_sizes, times = (length(sample_names)*n_reps)),
    beaker_number = "",
    beaker_w_sample_OD_mass = "",
    comments = "-"
  )

  skeleton_blank_correction_pipetting_datasheet <- tibble::tibble(
    date = date,
    experiment_name = experiment_name,
    blank_replication = 1:length(pipette_sizes),
    bouyoucous_cylinder_number = "",
    beaker_tare_set = "",
    beaker_number = "",
    beaker_OD_mass_w_sample = "",
    comments = "-"
  )

  sieving_datasheet <- tidyr::crossing(
    date = date,
    experiment_name = experiment_name,
    sample_name = sample_names,
    replication = 1:n_reps,
    microns = sieves_um,
    cumulative_mass_g = ""
    ) %>%
    dplyr::mutate(sample_number = rep(1:(length(sample_names)*n_reps), each = length(sieves_um))) %>%
    dplyr::select(.data$date:.data$replication, .data$sample_number, .data$microns, .data$cumulative_mass_g) %>%
    dplyr::arrange(.data$sample_number, dplyr::desc(.data$microns))

  all_datasheets <- list(
    psa_metadata = skeleton_psa_metadatasheet,
    psa_specimen_masses = skeleton_psa_specimen_masses_datasheet,
    psa_pipetting_data = skeleton_psa_pipetting_datasheet,
    blank_correction = skeleton_blank_correction_pipetting_datasheet,
    sieving_datasheet = sieving_datasheet
  )

  files_to_write <- all_datasheets %>%
    tibble::enframe(value = "x") %>%
    dplyr::select(-.data$name) %>%
    dplyr::mutate(file = paste0(
      new_folder,
      "/",
      experiment_name,
      "_",
      names(all_datasheets),
      "_",
      date,
      ".csv")
    )

  purrr::pwalk(.l = files_to_write, .f = readr::write_csv)

  message(crayon::green("Please verify that files were written to disk."))
}

