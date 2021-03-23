#' @title Populate empty data sheets
#'
#' @description Generates several empty .csv files with the correct column names
#'   and structure. Use for pipette method of particle size analysis.
#'
#' @details Calls various internals based on `protocol_ID` argument to
#' determine required structure of sheets.
#'
#' @param dir directory to write the skeleton files
#' @param date date test was **begun** by weighing air-dry specimens, ISO YYYY-MM-DD
#'   format
#' @param experiment_name experiment with which the samples are associated
#' @param sample_names a character vector of unique identifiers for the samples
#'   tested (for example, "EPK kaolin").
#' @param n_reps number of replicate specimens tested per sample
#' @param tin_tare_set a character string referencing a set of tin masses for
#'   water content determination
#' @param ... other arguments passed internally to [`pipette_sampling_datasheets()`],
#' `[hydrometer_sampling_datasheets()`], or [`sieving_datasheets()`]
#'
#' @details The date refers to the date the first step of the test was begun. As
#'   most soil tests span multiple days, this convention avoids any ambiguity
#'   about when they were weighed, tested, etc.
#'
#' @return Files are written to disk and a message is printed.
#' @export
#'

psa_datasheets <- function(dir, date, experiment_name, sample_names,
                           n_reps = 1, protocol_ID, tin_tare_set = "", beaker_tare_set, pipette_microns,
                           sample_beaker_numbers = "", sieves_microns,
                           ...){

  # code to ensure directory contains trailing slash

  if(stringr::str_sub(string = dir, start = -1) == "/"){
   directory <- dir} else{
     directory <- paste0(dir, "/")
   }


  new_folder <- paste0(directory, "psa-data_", date)

  if (dir.exists(new_folder)) {
    stop("The directory", new_folder, "already exists. Halting call to prevent over-write.")

  }

  ########

# Sheets required for any type of test ------------------------------------


  #  error message to prevent over-writing existing data
new_directory_path <- paste0(directory, "psa-data_", date)

  if(length(list.files(path = new_directory_path, ) != 0)){
    stop(glue::glue("\n There is already a folder in this directory titled `{new_directory_path}`. Call halted to prevent over-writing of the existing files."))
  }

  #  error message to ensure tin tare setargument is included

 if(missing(tin_tare_set)){
    stop("\n No tin tare set specified; please indicate which set of water content tins will be used for the test.")
  }

   psa_metadata <- tibble::tibble(
    date = date,
    experiment_name = experiment_name,
    sample_name = rep(sample_names, each= n_reps),
    replication = rep(1:n_reps, times = length(sample_names)),
    batch_sample_number = 1:(length(sample_names)*n_reps),
    protocol_ID = protocol_ID,
    comments = "-"
  )

  psa_specimen_masses <- tibble::tibble(
    date = date,
    experiment_name = experiment_name,
    sample_name = rep(sample_names, each = n_reps),
    replication = rep(1:n_reps, times = length(sample_names)),
    batch_sample_number = 1:(length(sample_names)*n_reps),
    air_dry_specimen_mass_for_test = "",
    comments = "-"
  )

  psa_hygroscopic_corrections <- tibble::tibble(
    date = date,
    experiment_name = experiment_name,
    sample_name = rep(sample_names, each = n_reps),
    replication = rep(1:n_reps, times = length(sample_names)),
    batch_sample_number = 1:(length(sample_names)*n_reps),
    tin_tare_set = tin_tare_set,
    tin_number = "",
    tin_w_wet_sample = "",
    tin_w_OD_sample = "",
    comments = "-"
  )

#############

# For tests including pipette analysis ------------------------------------

  # determine which data sheets are needed for the fines method
  # assign them as objects

  # this should be done with `switch` but I am having a hard time wrappign
  # my mind around how to write the statement....
  # there are only 3 conditions, so for now I'll just use if statements
  # based on the data objects I constructed to match each protocl to
  # one of the three fines methods


  # Leaving off 2021-03-21 end of day

  # Where I am stuck is trying to not jam un-needed arguments
  # into this function - I want to stash them in another function (I think).
  # Either have to set them to NULL as defaults, use the dots to pass in args from other functions, or (as I now feel is most likely) need to create a "constructor function" that returns a list of arguments to pass along with the date, experiment name, etc. to the other functions inside this one which operate conditionally. So instead of pipette_sampling_datasheets() being a function that returns a list of data frames, maybe it should be a "constructor function" which instead returns a list of arguments, and these get passed to yet another function to generate the actual tibbles? Then this latter function would be called inside `psa_datasheets()`to generate the needed objects which are subsequently gathered with the call to `mget()` at the end.
use_pipette <- protocol_ID %in% c(internal_data$pipette_invoking_method_IDs)

  if(use_pipette){
    # psa_pipette_sheets <- purrr::flatten(
      pipette_sheets <- pipette_sampling_datasheets(date = date, experiment_name = experiment_name,
                                  sample_names = sample_names,
                                  n_reps = n_reps,
                                  beaker_tare_set = beaker_tare_set,
                                  pipette_microns = pipette_microns,
                                  sample_beaker_numbers = sample_beaker_numbers,
                                  beaker_mass_w_OD_sample = "",
                                  blank_beaker_numbers = "",
                                  comments = "-",
                                  ...)
      # )

      # A very inelegant solution, but it works: rely on the 2 specific
      # return values of the internal call to pipette_sampling_datasheets
      # Then removes the originial list returned by that call
      # There has to be a better way with purrr::flatten or unlist
      # but these are not doing what I want and I have to move on

      psa_pipetting_data <- pipette_sheets$skeleton_psa_pipetting_datasheet

      psa_blank_correction_data <- pipette_sheets$skeleton_blank_correction_datasheet

      # This step probably isn't necessary now that I figured out how to subset
      # the list of all existing objectts based on being of class data frame instead
      # of list....but leaving in just to be sure
      rm(pipette_sheets)
 }




  # have not yet built the hydrometer_sampling_datasheets function
  # or the overall framework for the laser diffraction methods - for the latter,
  # it is likely nothing is needed here, as the data will be collected automatically
  # using Panalytical's software and then exported as a csv file

  # use_hydrometer <- protocol_ID %in% c(hydrometer_invoking_method_IDs)
#   if(use_hydrometer){psa_hydrometer_sheets <- hydrometer_sampling_datasheets(...)}

  # use_fines_laser_diffraction <- protocol_ID %in% c(fines_laser_diffraction_invoking_method_IDs)
  # if(use_fines_laser_diffraction){psa_fines_laser_diffraction_sampling_datasheets <- fines_laser_diffraction_sampling_datasheets(...)}

#############


# For tests including sieving, either manually or with the Ro-tap ------------



  use_sieves <- protocol_ID %in% c(internal_data$sieve_invoking_method_IDs)

  if(use_sieves){psa_sieving_datasheet <- sieving_datasheet(
    date = date, experiment_name = experiment_name, sample_names = sample_names,
    n_reps = n_reps, protocol_ID= protocol_ID, sieves_microns = sieves_microns)}

  all_objs <- mget(ls())

  # need something like this to keep only elements which have a data frame class:

  # I think this actually should be really simple with base R subsetting
  # but I just can't figure it out

  # I was so close with this attempt!! Just needed to iterate because all_opjbs is a list, then reduce the level because the [] subsetting expects an atomic vector instead ofa list
  # all_data_frames <- all_objs[is.data.frame(all_objs)]

  #####################

  # Last night spun my wheels for a couple HOURS on this.
  # This AM with a fresh mind I solved it in under 5 minutes
  # Lesson learned - GO TO BED!!


  # Two base r solutions, see https://stackoverflow.com/questions/6941506/subset-elements-in-a-list-based-on-a-logical-condition


  # Using this one ! The specification for sapply is confusing....the documentation
  # gives a HORRIBLE explanation! The value for the FUN.VALUE argument is supposed
  # to be a _template_ for the return value: for example, to expect a a double of length 4
  # you don't specify something like (mode = "double", length = 4) but use c(1:4). For my particular case I want a logical of length 1 for each list element so I use `TRUE` which
  # "shows" R what I am expecting. Tidyverse obviously is better than this
  # particularly brutal case but I am leaving in here as a learning experience
  # and just to use base R over tidyverse for once

 all_data_frames <- all_objs[vapply(X = all_objs,
                                    FUN = is.data.frame,
                                    FUN.VALUE = TRUE)]

  # An even more compact base R version which almost works but it's off by a level
  # all_datasheets <- mget(x = ls(sorted = FALSE), mode = "list")


  # An easier-to-write tidyverse solution with a typed map function !
  # all_data_frames <- all_objs[purrr::map_lgl(all_objs, is.data.frame)]

  # all_data_frames <- all_objs[as.logical(is.data.frame(all_objs))]


  dir.create(path = new_folder)

  path_to_write <- paste0(
    here::here(),
    "/",
    new_folder,
    "/",
    names(all_data_frames),
    "_",
    date,
    ".csv")

  files_to_write <- all_data_frames %>%
    tibble::enframe(value = "x") %>%
    dplyr::select(-.data$name) %>%
    dplyr::mutate(file = path_to_write)

  browser()

  purrr::pwalk(.l = files_to_write, .f = readr::write_csv)

  message(crayon::green("Please verify that files were written to disk."))
}

