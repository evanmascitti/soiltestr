#' Workhorse powering `hydrometer_schedule()`, which is the function
#' actually called by the user
#'
#' @inheritParams make_hydrometer_schedule
#'
#' @return tibble
#'
make_hydrometer_schedule <- function(start_time, batch_spacing, sample_numbers){

#  browser()


  sub_batches <- tibble::tibble(
    sample_number = .env$sample_numbers,
    sub_batch_sample_number = 1:4
  )


  # build schedule

 #  browser()

  schedule <-  tidyr::crossing(
  start_time = start_time,
  sample_number = sample_numbers,
  # t_min = batch_spacing + c(2, 4, 8, 15, 30, 55, (60 * c(seq(2, 12, 2), 24)))
  # t_min = batch_spacing + c(2, 8, 15, 30, (60 * c(2, 6, 10, 24)))
  t_min = c(2, 8, 15, 30, (60 * c(2, 6, 10, 24)))
  ) %>%  # I thin the batch spacing should not be included here, because it is accounted for in the start stirring time %>%
    dplyr::left_join(sub_batches, by = 'sample_number') %>%
    dplyr::mutate(
      start_stirring_time = dplyr::case_when(
        sub_batch_sample_number == 2L ~ start_time + 60*batch_spacing + 60*3,
        sub_batch_sample_number == 3L ~ start_time + 60*batch_spacing + 60*5,
        sub_batch_sample_number == 4L ~ start_time + 60*batch_spacing + 60*6,
        TRUE ~ start_time + 60*batch_spacing
      ), # allows one minute between stirring of each cylinder and accounts for
      # batches of 4
      stop_stirring_time  = start_stirring_time + 60*1, # one minute to stir each cylinder
      sample_time_min = dplyr::case_when(
        sub_batch_sample_number == 2L ~ t_min + 1,
        sub_batch_sample_number == 3L ~ t_min + 2,
        sub_batch_sample_number == 4L ~ t_min + 4,
        TRUE ~ t_min
      ),
      # sample_time_min = dplyr::case_when(
    #   sub_batch_sample_number == 2L ~ stir_time + 1,
    #   sub_batch_sample_number == 3L ~ stir_time + 4,
    #   sub_batch_sample_number == 4L ~ stir_time + 7,
    #   TRUE ~ t_min
    # ),
    sample_time_sec = 60* sample_time_min,
    #  clock_time = start_time + sample_time_sec
   clock_time = stop_stirring_time + sample_time_sec
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
#' @param start_time A datetime object representing an ISO clock time (including timezone). If `NULL` (the default), the start time is set to 7:59 AM on the day following the function call. This allows the stirring of the first sample to cease at 8 AM.
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
    hour = 7,
    min = 59,
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
   # dplyr::group_by(dplyr::across(-c(.data$start_time, start_stirring_time, stop_stirring_time, clock_time, sample_number))) %>%
   # tidyr::nest() %>%

   # debug here
# can't get the subtract extra seconds to work now ....for the time being, forget about it
 # full_schedule %>%
   # dplyr::mutate(
   #   # clock_time = purrr::map_dbl(data, ~subtract_extra_secs(clock_time = .$clock_time, sample_number = .$sample_number, start_stirring_time = .$start_stirring_time, stop_stirring_time = .$stop_stirring_time)),
   #   start_time = purrr::map_dbl(data, "start_time"),
   #   start_stirring_time = purrr::map_dbl(data, "start_stirring_time"),
   #   stop_stirring_time = purrr::map_dbl(data, "stop_stirring_time"),
   #   sample_number = purrr::map_int(data, "sample_number")) %>%
   dplyr::arrange(clock_time) %>%
   dplyr::mutate(
     dplyr::across(.cols = c(start_time, start_stirring_time, stop_stirring_time, clock_time),
                   .fns = lubridate::as_datetime, tz = Sys.timezone()),
     sampling_date = format(clock_time, "%F"),
     sampling_time = clock_time) %>%
 # ,
 #     stir_time = format(stir_time, "%I:%M %p"),
 #     sampling_time = format(clock_time, "%I:%M %p")
 #   ) %>%
   dplyr::ungroup() %>%
   dplyr::select(sample_number, .data$start_stirring_time, .data$stop_stirring_time, .data$sampling_date, .data$sampling_time)

 stir_times <- dplyr::distinct(full_schedule, .data$sample_number, .data$start_stirring_time, .data$stop_stirring_time) %>%
   dplyr::select(.data$sample_number, .data$start_stirring_time, .data$stop_stirring_time) %>%
   tidyr::pivot_longer(
     cols = c(.data$start_stirring_time, .data$stop_stirring_time),
     names_to = 'action',
     values_to = 'time'
   ) %>%
   dplyr::mutate(
     action = stringr::str_remove(stringr::str_replace_all(action, "_", " "), "\\s?time")
   )

 sampling_schedule <- full_schedule %>%
   dplyr::select(.data$sample_number, .data$sampling_date, .data$sampling_time)


 sampling_times_renamed <- sampling_schedule %>%
   dplyr::mutate(action = 'take sample') %>%
   dplyr::rename(time = sampling_time)




 # start_stirring_times <- stir_times %>%
 #   dplyr::mutate(action = 'start stirring') %>%
 #   dplyr::rename(time = stir_time)
 #
 # stop_stirring_times <- start_stirring_times %>%
 #   dplyr::mutate(action = 'stop stirring',
 #                 time = time + 60)

 # browser()

 all_times <- dplyr::bind_rows(sampling_times_renamed, stir_times) %>%
   dplyr::arrange(time) %>%
   dplyr::mutate(
     time = format(time, "%I:%M %p")) %>%
   dplyr::select(
     .data$sampling_date, .data$time, .data$sample_number, .data$action
   )

 return(all_times)

 # return_list <- list(
 #   stir_times = stir_times,
 #   sampling_schedule = sampling_schedule
 # )


  # return(return_list)

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
subtract_extra_secs <- function(start_stirring_time, stop_stirring_time, clock_time, sample_number){

  # browser()

  # exit early if elapsed time is < 3 hours

  if(as.numeric(lubridate::as.duration(clock_time - stop_stirring_time)) < 60*60*3){
    return(clock_time)
  }

  # if not, figure out the new time based on the sample number
  subtraction_lookup_tbl <- tibble::tibble(
    sample_number = 1:12,
#     secs_to_subtract = 60*c(0, 0, 2, 4, 6, 6, 8, 10, 7, 7, 9, 11)
# secs_to_subtract = 0
     # secs_to_subtract = 60*c(-1, 2, 4, 6, 14, 17, 19, 21, 24, 27, 29, 31)
secs_to_subtract = 60*c(-1, 2, 4, 6, 17, 20, 22, 24, 26, 27, 29, 31)
  )

  secs_to_subtract <- unlist(
    subtraction_lookup_tbl[subtraction_lookup_tbl$sample_number == sample_number, 'secs_to_subtract']
  )

  return_time <- clock_time - secs_to_subtract

  return(return_time)


}
