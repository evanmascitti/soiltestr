#' @title Populate empty data sheets
#'
#' @description Generates several empty .csv files with the correct column names
#'   and structure. Use for pipette method of particle size analysis.
#'
#' @param dir directory to write the skeleton file
#' @param date date test was **begun** by weighing air-dry specimens, yyyy-mm-dd
#'   format
#' @param experiment_name experiment with which the samples are associated
#' @param sample_names a character vector of unique identifiers for the samples
#'   tested (for example, "EPK kaolin")
#' @param n_reps number of replicate specimens tested per sample
#' @param pipette_sizes numeric vector of particle diameters for which to
#'   sample; typically 20, 5, 2, and 0.2 &mum)
#' @param sieves_um numeric or character vector containing the aperture size of
#'   the sieves to be used (in micrometers)
#' @param sample_beaker_numbers optional numeric vector of the beaker numbers to
#'   use for pipetting, see Details
#' @param blank_beaker_numbers optional numeric vector of the beaker numbers to
#'   use for pipetting, see Details
#' @param tin_tare_set a character string referencing a set of tin masses for
#'   water content determination
#' @param beaker_tare_set a character string referencing a set of beaker masses
#'   for pipetting
#'

#'
#' @details The date refers to the date the first step of the test was begun. As
#'   most soil tests span multiple days, this convention avoids any ambiguity
#'   about when they were weighed, tested, etc.
#'
#'   If a continuous sequence of beaker numbers is used (the usual case), it is
#'   helpful to set the `sample_beaker_numbers` argument to a numeric vector including
#'   these. If no numbers are given, this column defaults to empty and the
#'   numbers must be inputted manually after the file is generated. For example,
#'   if 3 specimens are being tested and the sampled particle diameters are 20,
#'   5, 2, and 0.2 &mu;, the user should use `sample_beaker_numbers = 1:12`. The
#'   blank should essentially be considered a separate analysis; in other words,
#'   its beaker numbers should not be interspersed within the sample beakers'
#'   sequence.
#'
#' @return files are written to disk and a message is printed
#' @export
#'

pipette_datasheets <- function(dir, date, experiment_name, sample_names,
                                  n_reps = 1, pipette_sizes = c(20, 5, 2, 0.2),
                                   sieves_um = c(4000, 2000, 1000, 500, 250, 150, 53),
                                   tin_tare_set = "", beaker_tare_set = "",
                                   sample_beaker_numbers = "", blank_beaker_numbers = ""){

  # code to ensure directory contains trailing slash

  if(stringr::str_sub(string = dir, start = -1) == "/"){
   directory <- dir} else{
     directory <- paste0(dir, "/")
  }

  ###

  #  error message to prevent over-writing existing data
new_directory_path <- paste0(directory, "pipette_w_sieves_data_", date)

  if(length(list.files(path = new_directory_path, ) != 0)){
    stop(glue::glue("\n There is already a folder in this directory titled `{new_directory_path}`. Call halted to prevent over-writing of the existing files."))
  }

  #  error message to ensure tin tare set and beaker tare set arguments are included

  if(missing(beaker_tare_set)){
    stop("\n No beaker tare set specified; please indicate which set of beakers will be used for the test.")
  }

  if(missing(tin_tare_set)){
    stop("\n No tin tare set specified; please indicate which set of water content tins will be used for the test.")
  }

   skeleton_psa_metadatasheet <- tibble::tibble(
    date = date,
    experiment_name = experiment_name,
    sample_name = rep(sample_names, each= n_reps),
    replication = rep(1:n_reps, times = length(sample_names)),
    comments = "-"
  )

  skeleton_psa_specimen_masses_datasheet <- tibble::tibble(
    date = date,
    experiment_name = experiment_name,
    sample_name = rep(sample_names, each = n_reps),
    replication = rep(1:n_reps, times = length(sample_names)),
    batch_sample_number = 1:(length(sample_names)*n_reps),
    air_dry_specimen_mass_for_test = "",
    comments = "-"
  )

  skeleton_psa_w_cont_correction_datasheet <- tibble::tibble(
    date = date,
    experiment_name = experiment_name,
    sample_name = rep(sample_names, each = n_reps),
    replication = rep(1:n_reps, times = length(sample_names)),
    batch_sample_number = 1:(length(sample_names)*n_reps),
    tin_tare_set = tin_tare_set,
    tin_number = "",
    tin_w_wet_sample = "",
    tin_w_OD_sample = "",
    comments = "-"
  )


  skeleton_psa_pipetting_datasheet <- tibble::tibble(
    date = date,
    experiment_name = experiment_name,
    sample_name = rep(sample_names, each = n_reps*length(pipette_sizes)),
    replication = rep(rep(1:n_reps, each = length(pipette_sizes), times = length(sample_names))),
    batch_sample_number = rep(1:(length(sample_names)*n_reps), each = length(pipette_sizes)),
    bouyoucos_cylinder_number = .data$batch_sample_number,
    beaker_tare_set = beaker_tare_set,
    microns = rep(pipette_sizes, times = (length(sample_names)*n_reps)),
    beaker_number = sample_beaker_numbers,
    beaker_mass_w_OD_sample = "",
    comments = "-"
  )

  skeleton_blank_correction_pipetting_datasheet <- tibble::tibble(
    date = date,
    experiment_name = experiment_name,
    blank_replication = 1:length(pipette_sizes),
    bouyoucos_cylinder_number = "",
    beaker_tare_set = beaker_tare_set,
    beaker_number = blank_beaker_numbers,
    beaker_mass_w_OD_sample = "",
    comments = "-"
  )

  sieving_datasheet <- tibble::tibble(
    date = date,
    experiment_name = experiment_name,
    sample_name = rep(sample_names, each = n_reps*length(sieves_um)),
    replication = rep(rep(1:n_reps, each = length(sieves_um)), length(sample_names)),
    batch_sample_number = rep(1:(length(sample_names)*n_reps), each = length(sieves_um)),
    microns = rep(sieves_um, times = (length(sample_names)*n_reps)),
    cumulative_mass_g = "",
    comments = "-"
  )

  all_datasheets <- list(
    metadata = skeleton_psa_metadatasheet,
    specimen_masses_data = skeleton_psa_specimen_masses_datasheet,
    hygroscopic_correction_data = skeleton_psa_w_cont_correction_datasheet,
    pipetting_data = skeleton_psa_pipetting_datasheet,
    blank_correction_data = skeleton_blank_correction_pipetting_datasheet,
    sieving_data = sieving_datasheet
  )

  new_folder <- paste0(directory, "pipette_w_sieves_data_", date)

  dir.create(path = new_folder)

  path_to_write <- paste0(
    new_folder,
    "/",
    names(all_datasheets),
    "_",
    date,
    ".csv")

  files_to_write <- all_datasheets %>%
    tibble::enframe(value = "x") %>%
    dplyr::select(-.data$name) %>%
    dplyr::mutate(file = path_to_write)

  purrr::pwalk(.l = files_to_write, .f = readr::write_csv)

  message(crayon::green("Please verify that files were written to disk."))
}

