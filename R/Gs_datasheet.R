#' Generate and write an empty data sheet for G~~s~~ testing
#'
#' Ensures consistent column names and easy analysis via `Gs_batch_analysis()`
#'
#' @param dir Directory in which file should be written
#' @param date Date of test initiation
#' @param experiment_name Character string identifying the set of trials
#' @param sample_name Character vector of unique sample identifiers
#' @param n_reps Number of replicate determinations
#' @param tin_tare_set Character string identifying set of tin tares, for the hygroscopic water content correction
#' @param tin_numbers Integer vector of tin numbers used for hygroscopic water content correction
#' @param bottle_set Character string identifying a set of pycnometer bottles
#' @param bottle_numbers Integer vector of bottle numbers used in the tests
#'
#' @return Writes file to disk and prints a message
#' @export
#'
Gs_datasheets <- function(dir, date = Sys.Date(), experiment_name, sample_name, n_reps, tin_tare_set = NULL, tin_numbers = NULL, bottle_set = NULL, bottle_numbers = NULL){

  # if arguments not provided, sub in an empty string
  tin_tare_set <- tin_tare_set %||% ""
  bottle_set <- bottle_set %||% ""

  tin_numbers <- tin_numbers %||% ""
  bottle_numbers <- bottle_numbers %||% ""

  # build tibble

  # browser()

  datasheet <- tibble::tibble(
    date = date,
    experiment_name = experiment_name,
    sample_name = rep(sample_name, each = n_reps),
    replication = rep(1:n_reps, times = length(.env$sample_name)),
    batch_sample_number = 1:(n_reps * length(.env$sample_name)),
    bottle_set = bottle_set,
    bottle_number = bottle_numbers,
    air_dry_specimen_mass = "",
    filled_bottle_post_boil_mass = "",
    water_temp_c = "",
    tin_tare_set = tin_tare_set,
    tin_number = tin_numbers,
    tin_w_wet_sample = "",
    tin_w_OD_sample = "",
    comments = "-")

  file_path <- here::here(
    dir,
    paste0('specific-gravity-data_', as.character(date), ".csv")
  )

  if(file.exists(file_path)){
    stop("File `", file_path, "`already exists. Halting function call to prevent over-write.",
         call. = F)
  }

  readr::write_csv(
    x = datasheet,
    file = file_path
  )


  # check that the file was written and then print a message
  if(file.exists(file_path)){
    message(crayon::green("Success! Wrote `", file_path, "` to disk."))
      }}
