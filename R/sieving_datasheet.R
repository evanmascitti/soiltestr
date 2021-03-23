#' Construct a sieving data sheet
#'
#' Accepts arguments fromo [`psa_datasheets()`] and constructs data frame of
#' appropriate length for the number of sieves used
#'
#' @inheritParams psa_datasheets
#' @param sieves_microns numeric vector of sieve opening diameters in microns
#' @param ... other arguments passed from [`psa_datasheets()`]
#'
#' @return Named list containing a single data frame
#'
sieving_datasheet <- function(date, experiment_name, sample_names,
                              n_reps, protocol_ID, sieves_microns,
                              ...) {


  sieving_sheet <- tibble::tibble(
  date = date,
  experiment_name = experiment_name,
  sample_name = rep(sample_names, each = n_reps*length(sieves_microns)),
  replication = rep(rep(1:n_reps, each = length(sieves_microns)), length(sample_names)),
  batch_sample_number = rep(1:(length(sample_names)*n_reps), each = length(sieves_microns)),
  microns = rep(sieves_microns, times = (length(sample_names)*n_reps)),
  cumulative_mass_g = "",
  comments = "-"
  )
  return(sieving_sheet)

  }
