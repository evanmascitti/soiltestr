#' Convert a directory of Mastersizer 3000 data to tidy format with
#' `batch_sample_number` as the identifier
#'
#' @param dir full path to directory
#' @param fmt format, one of `.xlsx`, `.csv`, or `.tsv`
#'
#' @return silently writes data to disk in `.csv` format
#' @export
#'
tidy_mastersizer <- function(dir, fmt = ".xlsx"){

  fmt <- match.arg(fmt, choices = c(".xlsx", ".csv", ".tsv"))


  all_files <- list.files(path = dir, pattern = fmt, full.names = T) %>%
    purrr::set_names(nm = stringr::str_extract(tools::file_path_sans_ext(basename(.)), "(?<=sample-)\\d+"))


  reading_fn <- switch(fmt,
  ".csv" = readr::read_csv,
  ".tsv" = readr::read_tsv,
  ".xlsx" = readxl::read_excel
    )

  reading_args <- all_files %>%
    tibble::enframe(name = "batch_sample_number",
                    value = "file")


  if(fmt == ".xlsx"){

   all_data <- reading_args %>%
     dplyr::rename(path = file) %>%
     dplyr::mutate(tbl = purrr::map(path, reading_fn))

  } else{

        all_data <- reading_args %>%
      dplyr::mutate(tbl = purrr::map(file, reading_fn))
  }

  # browser()

fines_vol_only <- all_data %>%
    tidyr::unnest(tbl) %>%
    dplyr::rename(
      microns = `Size (µm)`,
      vol_in_bin_normalized_to_total_fines = `% Volume In`
    ) %>%
  split(~batch_sample_number) %>%
  purrr::map(vol_in_bin_to_cumulative_percent_passing) %>%
  dplyr::bind_rows()
# write to disk


date <- stringr::str_extract(dir, "\\d{4}-\\d{2}-\\d{2}")

output_path <- fs::path(dir, paste0("psa-mastersizer-data_", date, ".csv"))

readr::write_csv(
  x = fines_vol_only,
  file = output_path
)


message(crayon::green("Succes! Write file ", output_path, " to disk." ))


}



#' Convert bin particle size data to cumulative % passing
#'
#' @param x data frame containing columns `microns` and `vol_in_bin`
#'
#' @return data frame containing `microns` and `cumulative_percent_passing`
#'
vol_in_bin_to_cumulative_percent_passing <- function(x){

  x %>%
    dplyr::mutate(
      cumulative_percent_passing_normalized_to_total_fines = 0.01 * cumsum(vol_in_bin_normalized_to_total_fines),
      # cumulative_percent_passing_normalized_to_total_fines =  1 - cumulative_percent_retained_normalized_to_total_fines
    # ) %>%
    # dplyr::select(-c(vol_in_bin_normalized_to_total_fines, cumulative_percent_retained_normalized_to_total_fines))
    ) %>%
    dplyr::select(-c(vol_in_bin_normalized_to_total_fines))

}
