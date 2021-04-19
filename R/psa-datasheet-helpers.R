#' Construct datasheets for pipette analysis
#'
#'
#' @details If a continuous sequence of beaker numbers is used (the usual case), it is
#'   helpful to set the `sample_beaker_numbers` argument to a numeric vector including
#'   these. If no numbers are given, this column defaults to empty and the
#'   numbers must be inputted manually after the file is generated. For example,
#'   if 3 specimens are being tested and the sampled particle diameters are 20,
#'   5, 2, and 0.2 &mu;, the user should use `sample_beaker_numbers = 1:12`. The
#'   blank should essentially be considered a separate analysis; in other words,
#'   its beaker numbers should not be interspersed within the sample beakers'
#'   sequence.
#'
#' @importFrom  rlang %||%
#'
#' @return a list of two data sheets, one for the actual sampling and one for
#' the blank correction
#' @export
#'
pipetting_datasheets <- function(){

  # import the existing arguments from the caller environment

  needed_objs <- mget(x =  c("date", "experiment_name", "sample_names",
                             "fines_diameters_sampled", "n_reps", "protocol_ID",
                             "beaker_tare_set", "bouyoucos_cylinder_numbers",
                             "pipette_beaker_numbers"),
                      envir = rlang::caller_env())

  list2env(x = needed_objs, envir = rlang::current_env())


  psa_pipetting_data <- tibble::tibble(
    date = date,
    experiment_name = experiment_name,
    protocol_ID = protocol_ID,
    sample_name = rep(sample_names, each = n_reps*length(fines_diameters_sampled)),
    replication = rep(rep(1:n_reps, each = length(fines_diameters_sampled), times = length(sample_names))),
    batch_sample_number = rep(1:(length(sample_names)*n_reps), each = length(fines_diameters_sampled)),
    bouyoucos_cylinder_number = rep(bouyoucos_cylinder_numbers %||% "", each = length(fines_diameters_sampled)),
    beaker_tare_set = beaker_tare_set %||% "",
    microns = rep(fines_diameters_sampled %||% "", times = (length(sample_names)*n_reps)),
    beaker_number = pipette_beaker_numbers %||% "",
    beaker_mass_w_OD_sample = "",
    comments = "-"
  )


  psa_blank_correction_data <- tibble::tibble(
    date = date,
    experiment_name = experiment_name,
    protocol_ID = protocol_ID,
    blank_replication = 1:length(fines_diameters_sampled),
    bouyoucos_cylinder_number = "",
    beaker_tare_set = beaker_tare_set %||% "",
    beaker_number = "",
    beaker_mass_w_OD_sample = "",
    comments = "-"
  )


  both_pipetting_sheets <- list(
    psa_pipetting_data= psa_pipetting_data,
    psa_blank_correction_data  = psa_blank_correction_data
  )

  return(both_pipetting_sheets)

}


#' Construct a sieving data sheet
#'
#' Accepts arguments fromo [`psa_datasheets()`] and constructs data frame of
#' appropriate length for the number of sieves used
#'
#' @param date the date the sample was initiated
#' @importFrom rlang %||%
#' @return Named list containing a single data frame
#'
sieving_datasheet <- function() {

  # browser()

  # import the existing arguments from the caller environment

  needed_objs <- mget(x = c("date", "experiment_name", "sample_names",
                            "n_reps", "protocol_ID", "coarse_diameters_sampled"),
                      envir = rlang::caller_env())

  list2env(x = needed_objs, envir = rlang::current_env())

  # build the tibble

  sieving_data <- tibble::tibble(
    date = date,
    experiment_name = experiment_name,
    protocol_ID = protocol_ID,
    sample_name = rep(sample_names, each = n_reps*length(coarse_diameters_sampled)),
    replication = rep(rep(1:n_reps, each = length(coarse_diameters_sampled)), length(sample_names)),
    batch_sample_number = rep(1:(length(sample_names)*n_reps), each = length(coarse_diameters_sampled)),
    microns = rep(coarse_diameters_sampled, times = (length(sample_names)*n_reps)),
    cumulative_mass_g = "",
    comments = "-"
  ) %>%
    dplyr::arrange(.data$batch_sample_number,
                   dplyr::desc(.data$microns))

  return(sieving_data)

}


#' Construct datasheets for pretreatment (OM, carbonates, and/or Fe-oxides)
#'
#' Allows a "blank" to be run and the actual sample's oven-dry mass to be corrected
#'
#' @inheritParams psa_datasheets
#' @return
#' @export
#'
pretreatment_datasheet <- function(){

  needed_objs <- mget(x = c("date", "experiment_name", "sample_names",
                            "n_reps", "protocol_ID"),
                      envir = rlang::caller_env())

  list2env(needed_objs, envir = rlang::current_env())

  psa_pretreatment_datasheet <- tibble::tibble(
    date = date,
    experiment_name = experiment_name,
    sample_name = rep(sample_names, each = n_reps),
    replication = rep(1:n_reps),
    batch_sample_number = 1:(length(sample_names)*n_reps),
    protocol_ID = protocol_ID,
    air_dry_specimen_mass_before_pretreatment = "",
    container_tare = "",
    container_mass_w_OD_sample = ""
   )

  return(psa_pretreatment_datasheet)
}

