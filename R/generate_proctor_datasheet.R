#' `r lifecycle::badge('experimental')`
#'
#' @title Generate a table to hold raw data in compaction tests.
#'
#' @description Generating a skeleton file permits a consistent data structure to be
#' maintained over time. It also eliminates the need to manually create a file
#' via Excel or another GUI, which is a nuisance and allows errors via
#' copy-paste or typos.
#'
#' @param soil_IDs a vector of unique identifiers for the soils to test
#' @param compaction_date the date the actual compaction test was performed
#'   (c.f. the mixing or sample prep)
#' @param efforts character vector of compaction conditions. Defaults to both
#'   standard and modified
#' @param write if TRUE, saves the table to disk as a `.csv` file
#' @param path location to save the file
#' @param cylinder_vol_cm3 volume of compaction mold
#' @param empty_cylinder_mass_g mass of compaction mold
#' @param n_cyl Number of compaction specimens to prepare per soil, defaults to
#' 5
#' @param ambient_temp_c The ambient temperature during the test in &deg;^C^.
#'   Used in [`add_physical_properties()`] to compute water density. Defaults to
#'   22 for filling data sheet; user should measure temperature during test and
#'   input before data collection. Be careful to record only one value during
#'   the test or [`add_physical_properties()`] will fail.
#'
#' @return a tibble if `write = FALSE`; otherwise writes file to disk and
#'   displays a message.
#' @example /inst/examples/generate_proctor_datasheet_example.R
#' @export
#'

generate_proctor_datasheet <- function(soil_IDs,
                                       compaction_date,
                                       efforts = c("standard", "modified"),
                                       write = FALSE,
                                       path = NULL,
                                       cylinder_vol_cm3 = 937.4,
                                       empty_cylinder_mass_g = 1484.5,
                                       n_cyl = 5,
                                       ambient_temp_c = 22)
{

  # error and warning messages if required fields are left blank

  if (missing(soil_IDs)) {
    stop('\n\nNo soil IDs provided. Please provide a single ID or a character vector of IDs.')
  }

  if (missing(compaction_date)) {
    stop('\n\n No date provided. Provide specify the date of the compaction test.')
  }

  if (!is.character(compaction_date)) {
    stop('\n\n The date you provided was passed as a type other than `character`. Please use quotation marks.')
  }

  # Make tibble

    datasheet <- tidyr::crossing(
      soil_ID = soil_IDs,
      effort= efforts,
      cylinder_number = 1:n_cyl
    ) %>%
      dplyr::mutate(
        compaction_date = compaction_date,
        cylinder_vol_cm3 = cylinder_vol_cm3,
        empty_cylinder_mass_g = empty_cylinder_mass_g,
        ambient_temp_c = ambient_temp_c,
        w_estimated = "",
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
        .data$soil_ID, .data$effort, dplyr::everything()
      ) %>%
      dplyr::arrange(dplyr::desc(.data$effort), .data$soil_ID, .data$cylinder_number)

    # return error message if file path was not supplied

  if(write == TRUE){
    if(missing(path) ){
      stop('\n\n No file path supplied. Please provide a file path.')
    }
    readr::write_csv(x= datasheet,
                     file = path)
  }

  return(if(write == TRUE) {message("File was written to disk.")} else {datasheet})

} # end of function
