#' Generate an empty data sheet
#'
#' Populates column names for thread rolling test
#'
#' @param date date of test, quoted and in yyyy-mm-dd format
#' @param experiment_name unique identifier for the experiment or analysis
#' @param sample_IDs character vector including all tested specimens. Usually a
#'   mix number corresponding to the present experiment (i.e. set of mixes)
#' @param dir directory to write the skeleton file (with trailing slash)
#' @param n_reps number of threads tested per sample
#'
#'
#' @details The date refers to the date the first step of the test was begun. As
#'   most soil tests span multiple days, this convention avoids any ambiguity
#'   about when they were weighed, tested, etc.
#'
#' @return file written to disk and a message is printed
#' @export
#'
new_PL_datasheet <- function(date, experiment_name, sample_IDs, dir, n_reps = 3){

  if(length (list.files(pattern = paste0(dir, date, "_PL_raw_data.csv")) ) != 0){
    stop("File with the same name already exists. Call halted to prevent over-writing existing datasheet.")
  } else{
    crayon::blue("writing file...")
  }

  skeleton_sheet <- tibble::tibble(
    date = date,
    experiment_name = experiment_name,
    sample_ID = rep(sample_IDs, each = n_reps),
    tin_tare_set = "",
    tin_number = "",
    tin_w_wet_sample = "",
    tin_w_OD_sample = "",
    comments = "-"
  )

  readr::write_csv(
    x = skeleton_sheet, file = paste0(dir, date, "_PL_raw_data.csv"))

  message(crayon::green("Please verify that file was written to disk."))
}


