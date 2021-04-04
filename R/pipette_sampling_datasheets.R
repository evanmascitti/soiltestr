#' Datasheets for pipette analysis
#'
#' @inheritParams psa_datasheets
#' @param pipette_microns numeric vector of particle diameters which will be sampled
#' @param beaker_tare_set unique identifier to write into sheets;
#' used by [`psa()`]
#' @param sample_beaker_numbers integer vector of beaker numbers from relevant set
#' @param ... arguments passed via call to [`psa_datasheets()`]
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
#' @return a list of two data sheets, one for the actual sampling and one for
#' the blank correction
#' @export
#'
pipette_sampling_datasheets <- function(date, experiment_name, sample_names,
                                        n_reps, pipette_microns, beaker_tare_set,
                                        sample_beaker_numbers,
                                        ...){


  skeleton_psa_pipetting_datasheet <- tibble::tibble(
    date = date,
    experiment_name = experiment_name,
    sample_name = rep(sample_names, each = n_reps*length(pipette_microns)),
    replication = rep(rep(1:n_reps, each = length(pipette_microns), times = length(sample_names))),
    batch_sample_number = rep(1:(length(sample_names)*n_reps), each = length(pipette_microns)),
    bouyoucos_cylinder_number = .data$batch_sample_number,
    beaker_tare_set = beaker_tare_set,
    microns = rep(pipette_microns, times = (length(sample_names)*n_reps)),
    beaker_number = sample_beaker_numbers,
    beaker_mass_w_OD_sample = "",
    comments = "-"
  )


  skeleton_blank_correction_datasheet <- tibble::tibble(
    date = date,
    experiment_name = experiment_name,
    blank_replication = 1:dplyr::if_else(
      length(pipette_microns) == 1,
      as.integer(1),
      length(pipette_microns)),
    bouyoucos_cylinder_number = "",
    beaker_tare_set = beaker_tare_set,
    beaker_number = "",
    beaker_mass_w_OD_sample = "",
    comments = "-"
  )


  pipette_sheets <- list(
    skeleton_psa_pipetting_datasheet= skeleton_psa_pipetting_datasheet,
    skeleton_blank_correction_datasheet  = skeleton_blank_correction_datasheet
  )

  return(pipette_sheets)

}
