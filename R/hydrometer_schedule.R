#' Workhorse powering `hydrometer_schedule()`, which is the function
#' actually called by the user
#'
#' @inheritParams make_hydrometer_schedule
#'
#' @return tibble
#'
make_hydrometer_schedule <- function(start_time, batch_spacing, sample_numbers){

  # browser()


  sub_batches <- tibble::tibble(
    sample_number = .env$sample_numbers,
    sub_batch_sample_number = 1:4
  )


  # build schedule

  schedule <-  tidyr::crossing(
  start_time = start_time,
  sample_number = sample_numbers,
  t_min = batch_spacing + c(2, 4, 8, 15, 30, 55, (60 * c(2:12, 24)))
  ) %>%
    dplyr::left_join(sub_batches, by = 'sample_number') %>%
    dplyr::mutate(
    sample_time_min = dplyr::case_when(
      sub_batch_sample_number == 2L ~ t_min + 1,
      sub_batch_sample_number == 3L ~ t_min + 4,
      sub_batch_sample_number == 4L ~ t_min + 7,
      TRUE ~ t_min
    ),
    sample_time_sec = 60* sample_time_min,
    clock_time = start_time + sample_time_sec
  ) %>%
  dplyr::arrange(clock_time)

  return(schedule)

}

#' \lifecycle{experimental}
#'
#' Calculate sampling times for hydrometer tests
#'
#' Built around the idea of using 3 sub-batches of 4 samples each. If any more are used in a sub-batch, too many times overlap.
#'
#' @param start_time A datetime object representing an ISO clock time (including timezone). If `NULL` (the default), the start time is set to 8 AM on the day following the function call.
#' @param batch_spacing Time interval in minutes between stirring the sub-batches, each comprising 4 samples. Recommended to use 70-min intervals.
#' @param sample_numbers List of sample numbers, with each element being a numeric vector of length 4.
#'
#' @importFrom rlang `%||%`
#' @return A tibble of sampling times corresponding to each sedimentation cylinder
#' @export
#'
hydrometer_schedule <- function(
  start_time = NULL,
  batch_spacing = c(0, 70, 140),
  sample_numbers = list(1:4, 5:8, 9:12)){

  if(length(batch_spacing) != length(sample_numbers)){
    stop("Length of `batch_spacing` and `sample_numbers` must be equal",
         call. = FALSE)
  }

  function_call_time <- lubridate::now()

  start_time <- start_time %||% lubridate::make_datetime(
    year = lubridate::year(function_call_time),
    month = lubridate::month(function_call_time),
    day = lubridate::day(function_call_time) + 1,
    hour = 8,
    min = 0,
    sec = 0,
    tz = Sys.timezone()
    )


# browser()

 full_schedule <- mapply(
   FUN = make_hydrometer_schedule,
   start_time = start_time,
   batch_spacing = batch_spacing,
   sample_numbers = sample_numbers,
   SIMPLIFY = FALSE
   ) %>%
    dplyr::bind_rows() %>%
   dplyr::group_by(dplyr::across(-c(start_time, clock_time, sample_number))) %>%
   tidyr::nest() %>%
   dplyr::mutate(
     clock_time = purrr::map_dbl(data, ~subtract_extra_secs(clock_time = .$clock_time, sample_number = .$sample_number, start_time = .$start_time)),
     start_time = purrr::map_dbl(data, "start_time"),
     sample_number = purrr::map_int(data, "sample_number")) %>%
   dplyr::arrange(clock_time) %>%
   dplyr::mutate(
     dplyr::across(.cols = c(start_time, clock_time),
                   .fns = lubridate::as_datetime, tz = Sys.timezone()),
     date = format(clock_time, "%F"),
     time = format(clock_time, "%I:%M %p")
   ) %>%
   dplyr::ungroup() %>%
   dplyr::select(sample_number, date, time)

  return(full_schedule)

}


# a helper to reduce wasted wait time after the third round is fully underway

#' Reduce gap between samples to 1 minute when the
#' third run has completed its first hour of sampling
#' and there is no longer any overlap between sampling times
#'
#' @param clock_time datetime object
#' @param sample_number numeric
#'
#' @return
#'
subtract_extra_secs <- function(start_time, clock_time, sample_number){


  # exit early if elapsed time is < 3 hours

  if(clock_time - start_time <= 60*60*3){
    return(clock_time)
  }

  # if not, figure out the new time based on the sample number
  subtraction_lookup_tbl <- tibble::tibble(
    sample_number = 1:12,
    secs_to_subtract = c(0, 0, 2:11)
  )

  secs_to_subtract <- unlist(
    subtraction_lookup_tbl[subtraction_lookup_tbl$sample_number == sample_number, 'secs_to_subtract']
  )

  return_time <- clock_time - secs_to_subtract

  return(return_time)


}
