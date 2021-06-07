
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
                             "pipette_beaker_numbers", "blank_correction_bouyoucos_cylinder", "Gs"),
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
    Gs = rep(Gs, each = n_reps*length(fines_diameters_sampled)),
    beaker_tare_set = beaker_tare_set %||% "",
    microns = rep(fines_diameters_sampled %||% "", times = (length(sample_names)*n_reps)),
    beaker_number = pipette_beaker_numbers %||% "",
    beaker_mass_w_OD_sample = "",
    comments = "-"
  )


  psa_pipette_blank_correction_data <- tibble::tibble(
    date = date,
    experiment_name = experiment_name,
    protocol_ID = protocol_ID,
    blank_replication = 1:length(fines_diameters_sampled),
    bouyoucos_cylinder_number = blank_correction_bouyoucos_cylinder,
    beaker_tare_set = beaker_tare_set %||% "",
    beaker_number = "",
    beaker_mass_w_OD_sample = "",
    comments = "-"
  )


  both_pipetting_sheets <- list(
    psa_pipetting_data= psa_pipetting_data,
    psa_pipette_blank_correction_data  = psa_pipette_blank_correction_data
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

###############################################################################

# hydrometer sheets -------------------------------------------------------

#' Determine which method to use for hydrometer blank correction
#'
#' @return Character, either "companion" or "temp_calibration"
#'
check_hydrometer_blank_method <- function(){

  # inherit the protocol ID from caller env and re-assign
  # so it can be used inside the dplyr filter data mask
   # browser()

  # this one is a bit tricky, could not get the inherits = TRUE argument
  # to work so trying rlang to search in the caller environment two levels up;
  # figured this out by using the browser call above and then repeatedly
  # calling rlang::caller_env() with different numbers and looking for the
  # object name in the returned character vector


  ID_for_hydrometer_blank_method <- get("protocol_ID", envir = rlang::caller_env(n = 2))


  # return a Boolean based on protocol ID and text from protocol summaries
  companion_measurement <- psa_protocols_summary %>%
    dplyr::filter(protocol_ID == ID_for_hydrometer_blank_method) %>%
    purrr::pluck("other_comments") %>%
    stringr::str_detect("blank computed w/ companion measurement")

  temp_calibration_measurement <-psa_protocols_summary %>%
    dplyr::filter(protocol_ID == ID_for_hydrometer_blank_method) %>%
    purrr::pluck("other_comments") %>%
    stringr::str_detect("blank computed w/ temperature calibration")

  # determine return value based on the above tests
  if(companion_measurement) return('companion')

  if(temp_calibration_measurement) return('temp_calibration')

  # should never reach this error message b/c one of the above
  # should always evaluate to TRUE ,but putting in for potential debugging help
  stop("No method for blank correction detected.
       Did you mis-match the protocol ID with the method used?")

}




#' Small helper for hydrometer datasheet function when companion measurements used
#'
#' Builds sheet structure knowing that companion measurements
#' are required
#'
#' @return A tibble
#' @seealso hydrometer_datasheets
#'
hydrometer_blank_correction_datasheet <- function(){

  # browser()

  # ran into a bit of trouble as I thought I might -
  # need to separately find the standard stuff and the variable named
  # "hydrometer_blank_method"; the former exists in the _caller_ of the caller
  # environment; i.e. 2 levels up, while the latter exists in the caller
  # environment of the current environment, i.e. 1 level up
  # Solved it by manually specifying the environment in which to look.
  # Still very unclear how the inherits argument works in mget.....maybe
  # due to the distinction between caller/parent/execution environments. Probably
  # have to read Hadley's environments chapter a third time and it would make
  # more sense

  needed_objs <- mget(
    x =  c(
      "date",
      "experiment_name",
      "protocol_ID",
      "fines_diameters_sampled",
      "blank_correction_bouyoucos_cylinder",
      "calgon_solution_ID",
      "hydrometer_ID"
    ),
    envir = rlang::caller_env(n = 2))

   list2env(x = needed_objs, envir = rlang::current_env())

   hydrometer_blank_method <-get("hydrometer_blank_method",
                                 envir = rlang::caller_env(n = 1))



  # Both options are created in this same function environment - this
  # reduces duplication by eliminating what would otherwise be an entirely
  # separate function to construct a nearly identical table

  # The only difference is that for the temperature calibration
  # method, the values for the hydrometer reading are given default
  # values  of "-", which when read back in by `psa()` will be come an NA
  # value. I think it is simpler this way; the sheet will still be named
  # properly and have the protocol ID right there to be able to verify
  # what was done and why the data are missing; by having the cell already
  # filled in it makes it clear that the data were not simply forgotten about

  # for redundancy, include the blank correction method as a column

   psa_hydrometer_blank_correction_w_companion_data  <- tibble::tibble(
    date = date,
    experiment_name = experiment_name,
    protocol_ID = protocol_ID,
    hydrometer_blank_method = hydrometer_blank_method,
    bouyoucos_cylinder_number = blank_correction_bouyoucos_cylinder,
    hydrometer_ID = hydrometer_ID,
    approx_ESD = fines_diameters_sampled %||% "",
    stir_date = "",
    stir_time = "",
    stir_AM_PM = "",
    sampling_date = "",
    sampling_time = "",
    sampling_AM_PM = "",
    water_temp_c = "",
    hydrometer_reading = rep("", times = length(fines_diameters_sampled)),
    meniscus_correction = "",
    comments = "-"
  )

  psa_hydrometer_blank_correction_w_temp_calibration_data  <-
    psa_hydrometer_blank_correction_w_companion_data %>%
    dplyr::mutate(hydrometer_reading = "-",
                  calgon_solution_ID = calgon_solution_ID %||% "") %>%
    dplyr::relocate(.data$calgon_solution_ID,
                    .after = .data$hydrometer_blank_method)

  # choose which one to return based on the value of the
  # hydrometer_blank_method variable which was inherited from parent
  # environment(s)

  if(hydrometer_blank_method == "companion") {
    return(psa_hydrometer_blank_correction_w_companion_data)
  }

  if(hydrometer_blank_method == "temp_calibration") {
    return(psa_hydrometer_blank_correction_w_temp_calibration_data)
  }
}

#' Construct two datasheets for hydrometer sampling
#'
#' The only hydrometer datasheet function actually called directly by
#' `psa_datasheets()`.
#' One sheet for the actual hydrometer measurements and a second one for
#' the blank corrections. The latter is populated based on the value
#' of a variable corresponding to the blank correction method employed
#' (either companion measurements or a calibration curve)
#'
#' @return A tibble
#'
#'
hydrometer_datasheets <- function(){



  # inherit required objects from calling environment

  needed_objs <- mget(
    x = c("date", "experiment_name", "sample_names", "n_reps", "protocol_ID", "bouyoucos_cylinder_numbers",
          "blank_correction_bouyoucos_cylinder", "fines_diameters_sampled", "Gs", "hydrometer_ID"),
    envir = rlang::caller_env())

  list2env(x = needed_objs, envir = rlang::current_env())

  ############

  # generate the blank correction datasheet

  # I guess there is a little duplication here because I am
  # generating the sheet inside the call to
  # `hydrometer_blank_correction_datasheet()` and also choosing
  # how to name it here. I can't think of a better way and feel
  # pretty confident this will work

hydrometer_blank_method <- check_hydrometer_blank_method()

  sheet_name <- dplyr:::if_else(
    hydrometer_blank_method == "companion",
    "psa_hydrometer_blank_correction_w_companion_data",
    "psa_hydrometer_blank_correction_w_temp_calibration_data")

  # generate the blank correction sheet and assign it to the current environment with the correct name

  sheet <-hydrometer_blank_correction_datasheet()
  assign(x = sheet_name, value = sheet)

######################

  # generate the main datasheet
  # for redundancy, include the blank correction method as a column

  # browser()

  psa_hydrometer_data  <- tibble::tibble(
    date = date,
    experiment_name = experiment_name,
    protocol_ID = protocol_ID,
    sample_name = rep(sample_names, each = n_reps*length(fines_diameters_sampled)),
    replication = rep(rep(1:n_reps, times = length(fines_diameters_sampled) * length(sample_names))),
    batch_sample_number = rep(1:(length(sample_names)*n_reps), times = length(fines_diameters_sampled)),
    bouyoucos_cylinder_number = rep(bouyoucos_cylinder_numbers %||% "", times = length(fines_diameters_sampled)),
    hydrometer_ID = hydrometer_ID,
    Gs = rep(Gs, each = n_reps * length(sample_names) * length(fines_diameters_sampled)),
    approx_ESD = rep(fines_diameters_sampled %||% "" , each = length(sample_names) * n_reps),
    stir_date = "",
    stir_time = "",
    stir_AM_PM = "",
    sampling_date = "",
    sampling_time = "",
    sampling_AM_PM = "",
    water_temp_c = "",
    hydrometer_reading = "",
    meniscus_correction = "",
    comments = "-"
  )


######################

  # collect the two objects to return; the blank correction sheet's name
  # is not known so have to use a pattern that will catch that one and also
  # the main hydrometer measurement sheet

  # Alternatively since they are both lists I could use the mode argument of mget
  # sticking with the pattern now just for curiosity to see if it works

  return(
    mget(x = ls(pattern = "psa_hydrometer_blank_correction_w\\w*_data|psa_hydrometer_data"))
  )

}


#############################################################################


# pre-treatments for OM, carbonates, Fe-oxides --------------------------------


#' Construct datasheets for pretreatment (OM, carbonates, and/or Fe-oxides)
#'
#' Allows a "blank" to be run and the actual sample's oven-dry mass to be corrected
#'
#' @return Tibble
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

