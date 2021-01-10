#' Generate an empty data sheet
#'
#' Populates column names for Casagrande liquid limit test
#'
#' @param date date of test, quoted and in yyyy-mm-dd format
#' @param expt_name unique identifier for the experiment or analysis
#' @param sample_ID character vector including all tested specimens. Usually a mix number corresponding to the present experiment (i.e. set of mixes)
#' @param dir directory to write the skeleton file
#'
#' @return file written to disk and a message is printed
#' @export
#'
new_LL_datasheet <- function(date, expt_name, sample_ID, dir){

  skeleton_sheet <- tibble::tibble(
    date = date,
    expt_name = expt_name,
    sample_ID = rep(sample_ID, each = 4),
    tin_tare_set = "",
    tin_number = "",
    blow_count = "",
    tin_w_wet_sample = "",
    tin_w_OD_sample = "",
    comments = "-"
  )

  readr::write_csv(
    x = skeleton_sheet, file = paste0(dir, "/", date, "_LL_raw_data.csv"))

  message(crayon::green("Please verify that file was written to disk."))
}


