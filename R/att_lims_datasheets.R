#' @title Populate empty data sheets
#'
#' @description Generates empty .csv files with the correct column names
#'   and structure. Use for liquid limit, plastic limit, and adhesion limit tests.
#'
#' @param dir directory under which to create the analysis folder
#' @param date date the tests were _initiated_ i.e. mixed with water
#' @param experiment_name experiment with which the samples are associated
#' @param sample_names a vector of unique identifiers for the samples tested,
#'   may be numeric or character
#' @param n_LL_trials number of speciments collected during liquid limit
#'   determination (minimum 3, defaults to 4)
#' @param n_PL_reps number of separate threads rolled during plastic limit test
#' @param adhesion_test whether the adhesion limit test will be performed
#' @param n_AL_reps number of separate determinations for adhesion limit test (if performed)
#' @param tin_tare_set character string identifying a set of tin masses
#'
#' @return files are written to disk and a message is printed
#' @export
#'
att_lims_datasheets <- function(dir, date, experiment_name, sample_names,
                                n_LL_trials = 4, n_PL_reps = 3, adhesion_test = T,
                                n_AL_reps = 3, tin_tare_set = ""
                                ){

  new_folder <- paste0(dir, "/atterberg_limits")

  if(length(list.files(path = dir, pattern = "atterberg_limits")) != 0){
    stop("\n There is already a folder titled `atterberg_limits`. Call halted to prevent over-writing of the existing files.")
  }

  dir.create(new_folder)

  LL_raw_data <- tibble::tibble(
    test_type = "LL",
    date = date,
    experiment_name = experiment_name,
    sample_name = rep(sample_names, each = n_LL_trials),
    sample_number = rep(1:length(sample_names), each = n_LL_trials),
    tin_number = "",
    blow_count = "",
    tin_w_wet_sample = "",
    tin_w_OD_sample = "",
    tin_tare_set = tin_tare_set,
    comments = "-"
  )

  readr::write_csv(x = LL_raw_data, file = paste0(new_folder, "/", "LL_raw_data_", date, ".csv"))

  PL_raw_data <- tibble::tibble(
    test_type = "PL",
    date = date,
    experiment_name = experiment_name,
    sample_name = rep(sample_names, each = n_PL_reps),
    sample_number = rep(1:length(sample_names), each = n_PL_reps),
    replicaton = rep(1:n_PL_reps, times = length(sample_names)),
    tin_number = "",
    tin_w_wet_sample = "",
    tin_w_OD_sample = "",
    tin_tare_set = tin_tare_set,
    comments = "-"
  )

  readr::write_csv(x = PL_raw_data, file = paste0(new_folder, "/", "PL_raw_data_", date, ".csv"))

  if(adhesion_test == TRUE){
  AL_raw_data <-tibble::tibble(
    test_type = "AL",
    date = date,
    experiment_name = experiment_name,
    sample_name = rep(sample_names, each = n_AL_reps),
    sample_number = rep(1:length(sample_names), each = n_AL_reps),
    replicaton = rep(1:n_AL_reps, times = length(sample_names)),
    tin_number = "",
    tin_w_wet_sample = "",
    tin_w_OD_sample = "",
    tin_tare_set = tin_tare_set,
    comments = "-"
  )

  readr::write_csv(x = AL_raw_data, file = paste0(new_folder, "/", "AL_raw_data_", date, ".csv"))
  }

  message(crayon::green("Please verify that files were written to disk."))

}
