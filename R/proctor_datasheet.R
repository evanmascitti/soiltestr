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
#'   directly from [`proctor_prep()`]. Must be of class `proctor_prep_sheet`. Includes sample metadata such as test
#'   date, mix names, efforts, etc.
#'   @param date Date of compaction test. If `NULL` (the default), inherits date from `prep_sheet` argument.
#' @param tin_tare_set Character string (length 1) which identifies the set of tin tare
#'   measurements to use during subsequent data analyses. If `NULL` (the default), an empty string is used.
#' @param tin_numbers Numeric. Length must equal total number of experimental units in `prep_sheet`. If `NULL` (the default), an empty string is used.
#' @param write Defaults to `TRUE` and saves empty data sheet as a `.csv` file. Will not over-write an
#'  existing file.
#' @param directory Directory in which to save the file.
#' @param path If `"auto"`, constructs a file name from the date column of `prep_sheet`
#'   and the `directory` argument; otherwise specify a path to save the file
#'   (including relative directory)
#' @param ambient_temp_c Ambient temperature during the test in &deg;C. Used
#'   to compute water density during data analysis (see
#'   [`add_physical_properties()`]). Defaults to 20&deg;C for filling data sheet; user
#'   should measure temperature during test and input before data collection. Be
#'   careful to record only one value during the test or
#'   [`add_physical_properties()`] will fail.
#'
#' @return A tibble of relevant data. Also writes file to disk when `write = TRUE`.
#' @example /inst/examples/proctor_datasheet_example.R
#' @export
#'
#' @importFrom rlang `%||%`
#'

proctor_datasheet <- function(prep_sheet = NULL,
                              date = NULL,
                              tin_tare_set = NULL,
                              tin_numbers = NULL,
                              ambient_temp_c = 20,
                              write = TRUE,
                              directory = NULL,
                              path = "auto"
                              ) {


  # require user to provide a prep sheet

  # if(!class(prep_sheet) %in% 'proctor_prep_sheet'){
  #   stop("Argument supplied to `prep_sheet` is not a `proctor_prep_sheet` object. Use `soiltestr::proctor_prep()` to construct this argument.")
  # }

  # if(is.null(prep_sheet) & all(is.null(sample_name, effort, est_w_opt))){
  #   stop("All arguments are null; please provide either a `prep_sheet` object or supply all of `sample_name`, `effort`, and `est_w_opt`.")
  # }

  # browser()

# assign variables for anything that is used whether the table is passed in
  # as a proctor_pep object or not

  tin_tare_set <- tin_tare_set %||% ""
  tin_numbers <- tin_numbers %||% ""
  new_date <- date %||% unique(prep_sheet$date)


  if(nrow(prep_sheet) == 0L){
    stop("Prep sheet argument has length 0.", call. = FALSE)
  }


  # if (!is.null(prep_sheet)) {


    # obeject-oriented stuff
    # check class, etc.
    # then mutate the sheet into what is needed


    #   }



  # Build tibble if prep sheet object not supplied


   # effort <- match.arg(
   #      arg = effort,
   #      choices = c('reduced', 'standard', 'modified'),
   #      several.ok = TRUE)
   #
   #   # tibble::tibble(
   #   #   sample_name = rep(sample_name, each = length(effort) * n_cylinders),
   #   #   effort = rep(effort, each = length(sample_name) * n_cylinders
   #   # )
   #
   # est_w_opts <- tibble::tibble(
   #   sample_name = sample_name,
   #   est_std_w_opt = est_w_opt
   # )
   #
   # tidyr::crossing(
   #   sample_name = sample_name,
   #   effort = effort
   # ) %>%
   #   dplyr::left_join(est_w_opts, by = 'sample_name') %>%
   #   dplyr::mutate(
   #     w_target = dplyr::if_else()
   #   )



    ###################


# browser()

  data_tibble <- prep_sheet %>%
     dplyr::mutate(
       date = new_date,
        ambient_temp_c = ambient_temp_c,
        tin_tare_set = tin_tare_set,
        w_target = as.character(round(.data$w_target, digits = 3)),
        mold_ID = "",
        filled_mold_g = "",
        tin_number = tin_numbers,
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

    if(missing(directory) && path == "auto"){
      stop('\n\nNo directory provided, please specify a directory OR manually provide a full `path`')
    }

    if(length(unique(data_tibble$date)) > 1){
      stop('\n\n Multiple dates found in data sheet, cannot write to a single file. Please split `prep_sheet` by date and save separately.')
    }

    # browser()

    if(path == "auto"){
      proctor_file_path <- fs::path(directory, glue::glue("proctor-data_{unique(data_tibble$date)}.csv"))
      } else{
        proctor_file_path <- path
      }

    # write file to disk but
    # only if it does not already exist
    # if it _does_ exist, warn user and don't write

    if(!file.exists(proctor_file_path)){
      readr::write_csv(x= data_tibble,
                       file = proctor_file_path)
      message(crayon::green("Please verify that file", proctor_file_path, "was correctly written to disk."))
    } else{
      warning("File ", proctor_file_path, " already exists. Nothing written to disk.")
      }


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

  return(data_tibble)

} # end of function




#' Format a data sheet for use in R Markdown documents
#'
#' @param x a proctor data sheet
#' @param ... arguments passed to `kableExtra::kbl()`
#'
#' @return A `kableExtra` kable object
#' @export
#'
format_proctor_datasheet <- function(x, format = 'latex', ...) {

  x %>%
    kableExtra::kbl(format = format, ...)


}
