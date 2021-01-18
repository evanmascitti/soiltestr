#' Generate an empty data sheet
#'
#' Populates column names for Casagrande liquid limit test
#'
#' @param date date of test, quoted and in yyyy-mm-dd format
#' @param experiment_name unique identifier for the experiment or analysis
#' @param sample_names character vector including all tested specimens. Usually a
#'   mix number corresponding to the present experiment (i.e. set of mixes)
#' @param dir directory to write the skeleton file (with trailing slash)
#' @param n_trials Number of data points collected per test; minimum is 3 (one with 25-35
#'   blows, one with 20-30 blows, and one with 15-25 blows)
#'
#' @details The date refers to the date the first step of the test was begun. As
#'   most soil tests span multiple days, this convention avoids any ambiguity
#'   about when they were weighed, tested, etc.
#'
#' @return file written to disk and a message is printed
#' @export
#'
new_LL_datasheet <- function(date, experiment_name, sample_names, dir, n_trials = 4){

  if(length (list.files(pattern = paste0(dir, date, "_LL_raw_data.csv")) ) != 0){
    stop("File with the same name already exists. Call halted to prevent over-writing existing datasheet.")
  } else{
    crayon::blue("writing file...")
  }

   skeleton_sheet <- tibble::tibble(
    date = date,
    experiment_name = experiment_name,
    sample_name = rep(sample_names, each = n_trials),
    sample_number = 1:length(sample_names),
    tin_tare_set = "",
    tin_number = "",
    blow_count = "",
    tin_w_wet_sample = "",
    tin_w_OD_sample = "",
    comments = "-"
  )

  readr::write_csv(
    x = skeleton_sheet, file = paste0(dir, date, "_LL_raw_data.csv"))

  message(crayon::green("Please verify that file was written to disk."))
}


