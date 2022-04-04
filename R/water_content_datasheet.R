#' Generate pre-populated .csv for computing extant water content
#'
#'
#'
#' @param date date of test (YYYY-MM-DD format)
#' @param dir path to directory in which file should be written
#' @param sample_name character vector of unique sample identifiers
#' @param tin_tare_set character (length 1) of tin tare identifier
#' @param tin_number character vector of tin numbers; must be of same length as `sample_name`
#' @param overwrite if the file already exists, should it be over-written?
#'
#' @return Prints message and writes file to disk
#' @export
#'
#' @importFrom rlang `%||%`
water_content_datasheet <- function(date, sample_name, dir, tin_tare_set = NULL, tin_numbers = NULL, overwrite = FALSE){

  tbl <- tibble::tibble(
    date = date,
    sample_name = sample_name,
    tin_tare_set = tin_tare_set,
    tin_number = tin_numbers %||% "",
    tin_w_wet_sample = "",
    tin_w_OD_sample = "",
    comments = "-"
  )

  out_path <- fs::path(
    dir, paste0('water-content-data_', date, ".csv")
  )

  if(all(file.exists(out_path) & !overwrite )){
    stop("File ", out_path, " already exists; halting function call to prevent over-write.")
  } else{

    readr::write_csv(
      x = tbl,
      file = out_path
    )

    message(crayon::green(
      "Success! File ", out_path, " was successfully written to disk. "
    ))

  }






}
