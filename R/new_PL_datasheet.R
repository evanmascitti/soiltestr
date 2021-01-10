#' Generate an empty data sheet
#'
#' Populates column names for thread rolling test
#'
#' @param date date of test, quoted and in yyyy-mm-dd format
#' @param expt_name unique identifier for the experiment or analysis
#' @param sample_ID character vector including all tested specimens. Usually a mix number corresponding to the present experiment (i.e. set of mixes)
#' @param n_reps number of threads tested per sample
#' @param dir directory to write the skeleton file
#'
#' @return file written to disk and a message is printed
#' @export
#'
new_PL_datasheet <- function(date, expt_name, sample_ID, n_reps = 3, dir){

  skeleton_sheet <- tibble::tibble(
    date = date,
    expt_name = expt_name,
    sample_ID = rep(sample_ID, each = n_reps),
    tin_tare_set = "",
    tin_number = "",
    tin_w_wet_sample = "",
    tin_w_OD_sample = "",
    comments = "-"
  )

  readr::write_csv(
    x = skeleton_sheet, file = paste0(dir, "/", date, "_PL_raw_data.csv"))

  message(crayon::green("Please verify that file was written to disk."))
}


