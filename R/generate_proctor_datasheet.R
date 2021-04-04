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
#' @param prep_sheet A data frame. It is recommended to pipe this argument
#'   directly from [`proctor_prep()`].
#' @param dir Directory in which to save the file, should end with `"/"`
#' @param path If `"auto"`, constructs a file name from the date column of `df`
#'   and the `dir` argument; otherwise specify a path to save the file
#'   (including relative directory)
#' @param write Defaults to `TRUE` and saves data sheet as a .csv; if set to
#'   `FALSE`, returns a tibble instead of writing to disk
#' @param cylinder_vol_cm3 Volume of compaction mold
#' @param empty_cylinder_mass_g Mass of compaction mold
#' @param ambient_temp_c Ambient temperature during the test in &deg;C. Used
#'   to compute water density during data analysis (see
#'   [`add_physical_properties()`]). Defaults to 22 for filling data sheet; user
#'   should measure temperature during test and input before data collection. Be
#'   careful to record only one value during the test or
#'   [`add_physical_properties()`] will fail.
#' @param tin_tares_lookup quoted string which identifies the set of tin tare
#'   measurements to use during subsequent data analyses. These are not used by
#'   this function, but including this information in the raw data file saves a
#'   step later in the analysis pipeline.
#'
#' @return Writes file to disk if `write = TRUE`; returns a tibble if `write =
#'   FALSE` .
#' @example /inst/examples/generate_proctor_datasheet_example.R
#' @export
#'

generate_proctor_datasheet <- function(prep_sheet,
                                       dir,
                                       path = "auto",
                                       write = TRUE,
                                       cylinder_vol_cm3 = 937.4,
                                       empty_cylinder_mass_g = 1484.5,
                                       ambient_temp_c = 22,
                                       tin_tares_lookup = NULL) {


  # error message if required field is left blank

  if (missing(prep_sheet)) {
    stop('\n\nNo prep sheet provided.')
  }


  # Make tibble

    datasheet <- prep_sheet %>%
      dplyr::select(c(.data$sample_name, .data$effort, .data$date, .data$cylinder_number, .data$w_target)) %>%
      dplyr::mutate(
        cylinder_vol_cm3 = cylinder_vol_cm3,
        empty_cylinder_mass_g = empty_cylinder_mass_g,
        ambient_temp_c = ambient_temp_c,
        tin_tares_lookup = tin_tares_lookup,
        w_target = as.character(round(.data$w_target, 3)),
        filled_cylinder_mass_g = "",
        penetrometer1 = "",
        penetrometer2 = "",
        penetrometer3 = "",
        tin_number = "",
        tin_w_wet_sample = "",
        tin_w_OD_sample = "",
        comments = ""
      ) %>%
      dplyr::select(
        .data$sample_name,
        .data$date,
        .data$effort,
        .data$cylinder_vol_cm3,
        .data$empty_cylinder_mass_g,
        .data$ambient_temp_c,
        tin_tares_lookup,
        .data$cylinder_number,
        .data$w_target,
        .data$filled_cylinder_mass_g,
        .data$penetrometer1,
        .data$penetrometer2,
        .data$penetrometer3,
        .data$tin_number,
        .data$tin_w_wet_sample,
        .data$tin_w_OD_sample,
        .data$comments) %>%
      dplyr::arrange(
        dplyr::desc(.data$effort),
        .data$sample_name,
        .data$cylinder_number)


# write file to disk if write ==  TRUE

  if(write == TRUE){

    # return error message if file path was not supplied

    if(missing(dir) && path == "auto"){
      stop('\n\nNo directory provided, please specify a directory OR manually provide a full `path`')
    }

    if(length(unique(datasheet$date)) > 1){
      stop('\n\n Multiple dates found in data sheet, cannot write to a single file. Please split `prep_sheet` by date and save separately.')
    }

    if(path == "auto"){
      file_path <- paste0(dir, unique(datasheet$date), "_proctor_raw_data.csv")
      } else{
        file_path <- path
      }

    readr::write_csv(x= datasheet,
                     file = file_path)
  }

    # return message if file was written to disk and return tibble if not

  return(if(write == TRUE) {message(crayon::green("Please verify that file was correctly written to disk."))} else {datasheet})

} # end of function
