#' `r lifecycle::badge('experimental')`
#'
#' @title Generate a table to hold raw data in compaction tests.
#'
#' @description Generating a skeleton file permits a consistent data structure
#'   to be maintained over time. It also eliminates the need to manually create
#'   a file via Excel or another GUI, which is a nuisance and allows errors via
#'   copy-paste or typos. The user may simply specify a directory and the file
#'   is automatically written to disk with a sensible name(
#'   `~/<directory>/<date>_<proctor_raw_data>`). Alternatively, a custom
#'   file name can be specified.
#'
#'
#' @param prep_sheet A data frame. It is recommended to pipe this argument
#'   directly from [`proctor_prep()`]. Includes sample metadata such as test
#'   date, mix names, efforts, etc.
#' @param dir Directory in which to save the file, should end with `"/"`
#' @param path If `"auto"`, constructs a file name from the date column of `df`
#'   and the `dir` argument; otherwise specify a path to save the file
#'   (including relative directory)
#' @param write Defaults to `TRUE` and saves data sheet as a .csv; if set to
#'   `FALSE`, returns a tibble instead of writing to disk
#' @param printable Logical. Whether to also create a printable pdf version of the data sheet for use during data collection (i.e. pen and paper)
#' @param empty_cylinder_mass_g Mass of compaction mold
#' @param ambient_temp_c Ambient temperature during the test in &deg;C. Used
#'   to compute water density during data analysis (see
#'   [`add_physical_properties()`]). Defaults to 22&deg;C for filling data sheet; user
#'   should measure temperature during test and input before data collection. Be
#'   careful to record only one value during the test or
#'   [`add_physical_properties()`] will fail.
#' @param tin_tares_set quoted string which identifies the set of tin tare
#'   measurements to use during subsequent data analyses. These are not used by
#'   this function, but including this information in the raw data file saves a
#'   step later in the analysis pipeline.
#'
#' @return Writes file to disk if `write = TRUE`; returns a tibble if `write =
#'   FALSE` .
#' @example /inst/examples/proctor_datasheet_example.R
#' @export
#'

proctor_datasheet <- function(prep_sheet,
                              dir,
                              tin_tare_set = NULL,
                              tin_numbers = NULL,
                              path = "auto",
                              ambient_temp_c = 22,
                              write = TRUE,
                              printable = TRUE) {


  # browser()

  # error message if required field is left blank

  if (missing(prep_sheet)) {
    stop('\n\nNo prep sheet provided.')
  }

  tin_tare_set <- tin_tare_set %||% ""
  tin_numbers <- tin_numbers %||% ""


  # Build tibble

    data_tibble <- prep_sheet %>%
     dplyr::mutate(
        ambient_temp_c = ambient_temp_c,
        tin_tare_set = tin_tare_set,
        w_target = as.character(round(.data$w_target, digits = 3)),
        mold_ID = "",
        filled_mold_g = "",
        tin_number = "",
        tin_w_wet_sample = "",
        tin_w_OD_sample = "",
        comments = "-"
      ) %>%
      dplyr::select(
        .data$date,
        .data$sample_name,
        .data$effort,
        .data$tin_tare_set,
        .data$ambient_temp_c,
        .data$w_target,
        .data$cylinder_number,
        .data$mold_ID,
        .data$filled_mold_g,
        .data$tin_number,
        .data$tin_w_wet_sample,
        .data$tin_w_OD_sample,
        .data$comments) %>%
      dplyr::arrange(
        .data$sample_name,
        dplyr::desc(.data$effort),
        .data$cylinder_number)


# write file to disk if write ==  TRUE

  if(write == TRUE){

    # return error message if file path was not supplied

    if(missing(dir) && path == "auto"){
      stop('\n\nNo directory provided, please specify a directory OR manually provide a full `path`')
    }

    if(length(unique(data_tibble$date)) > 1){
      stop('\n\n Multiple dates found in data sheet, cannot write to a single file. Please split `prep_sheet` by date and save separately.')
    }

    if(path == "auto"){
      proctor_file_path <- fs::path(dir, glue::glue("proctor-data_{unique(data_tibble$date)}.csv"))
      } else{
        proctor_file_path <- path
      }

    readr::write_csv(x= data_tibble,
                     file = proctor_file_path)
  }

# if(printable == TRUE){
#
#  rds_path <- glue::glue("proctor-data_{unique(data_tibble$date)}.rds")
#
#   # write the datasheet as an rds file
#   readr::write_rds(
#     x = data_tibble,
#     file = rds_path
#   )
#
#
#   # generates the Rmd file
#
#   rmd_path <- glue::glue("proctor-data-printable_{unique(data_tibble$date)}.Rmd")
#
#   rmarkdown::draft(
#     file = rmd_path,
#     template = 'printable-proctor-datasheet',
#     package = 'soiltestr',
#     edit = FALSE,
# )
#
#   # sets the parameter in the YAML to match the rds tibble
#   # generated above and then renders the Rmd file into a pdf
#
#   rmarkdown::render(
#     input = rmd_path,
#     params = list(rds_file = rds_path),
#     envir = rlang::current_env()
#     )
#
# }

    # return message if file was written to disk and return tibble if not

  return(if(write == TRUE) {message(crayon::green("Please verify that file was correctly written to disk."))} else {data_tibble})

} # end of function
