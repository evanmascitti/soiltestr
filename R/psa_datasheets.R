#' @title Populate empty data sheets
#'
#' @description Generates several empty .csv files with the correct column names
#'   and structure. Use for pipette method of particle size analysis.
#'
#' @details Calls various internals based on `protocol_ID` argument to determine
#'   required structure of sheets.
#'
#' @param dir Character. Directory to write the empty data files.
#' @param date Character. Date test was **begun** by weighing air-dry specimens,
#'   ISO YYYY-MM-DD format
#' @param protocol_ID Character (integers will be coerced), length 1. Unique
#'   identifier for the method employed; see [`psa_protocols`]
#' @param experiment_name Character, length 1. Experiment with which the samples
#'   are associated.
#' @param sample_names Character. Unique identifiers for the samples tested (for
#'   example, "EPK kaolin").
#' @param n_reps  Integer, length 1. Number of replicate specimens tested per
#'   sample
#' @param tin_tare_set Character, length 1. Set of tin masses for water content
#'   determination.
#' @param beaker_tare_set Character, length 1. Set of beaker masses for
#'   pipetting
#' @param pipette_beaker_numbers Integer (optional). Vector of the beakers used
#'   for pipette weighing.
#' @param bouyoucos_cylinder_numbers Integer (optional). Vector of the test
#'   cylinders used for pipette weighing. Important for hydrometer analysis due
#'   to width correction.
#' @param blank_correction_bouyoucos_cylinder Integer (optional). Cylinder number
#' used for the blank correction; not so important for pipette analysis but needed
#' for hydrometer to reference cross-sectional area which is needed for
#' calculation of effective depth
#' @param hydrometer_ID Integer of length 1 identifying which hydrometer is used for the test. The ID is used to link to a data object in the `asi468` packgae which contains information about the hydrometer's dimensions. Ignored by protocols not using hydrometer analysis.
#' @param Gs Double vector of matching length to `sample_names`. Specific gravity of soil solids (assumed or measured). If no value entered, defaults to 2.70.
#' @param ... Currently unused, reserved for expansion (potentially to pass other arguments to internal functions
#'   `pipette_sampling_datasheets()`, `hydrometer_sampling_datasheets()`, or
#'   `sieving_datasheets()`)
#'
#' @details The date refers to the date the first step of the test was begun. As
#'   most soil tests span multiple days, this convention avoids any ambiguity
#'   about when they were weighed, tested, etc.
#'
#' @return Files are written to disk and a message is printed.
#' @export
#'

psa_datasheets <- function(
  dir,
  date,
  protocol_ID,
  experiment_name,
  sample_names,
  n_reps = 1,
  tin_tare_set = "",
  beaker_tare_set = NULL,
  pipette_beaker_numbers = NULL,
  bouyoucos_cylinder_numbers = NULL,
  blank_correction_bouyoucos_cylinder = NULL,
  hydrometer_ID = NULL,
  Gs = NULL,
  calgon_solution_ID = NULL,
  ...){


# argument checking  ------------------------------------------------------


  # coerce protocol ID to character type in case user supplies it as an integer

  protocol_ID <- as.character(protocol_ID)

#browser()

  # if a vector is supplied for Gs, check that it is either a single value or the same length as the number of samples
  if(!is.null(Gs) && (!length(Gs) %in% c(1, length(sample_names)) )){
    stop("Length of vectors supplied for `sample_names` and `Gs` are of different lengths.")
  }

  # if the supplied Gs vector is length 1, repeat it as many times as there are sample names. The above error check makes sure to exit if the vector is supplie and is the wrong length, so no need to check that conditions again.
  if(length(Gs)  == 1L ){

    Gs <- rep(Gs, times = length(sample_names))

  } else{

    # if vector is supplied or created in the above statement, capture it....
    # otherwise (meaning it is NULL), default to 2.7

    Gs <- Gs %||% rep(2.7, times = length(sample_names))

  }



    if(protocol_ID %in% hydrometer_invoking_method_IDs & is.null(hydrometer_ID)){
    stop("This protocol requires a value for `hydrometer_ID` but you did not provide one.")
  }


  # create path to new folder and check if it already exists

  new_folder <- here::here(dir, paste0("psa-data_", date))

  if (dir.exists(new_folder)) {
    stop("The directory ",
         new_folder, " already exists. Halting call to prevent over-write.",
         call. = F)

  }


  # check that date was entered in correct format

  if(!stringr::str_detect(date, "^\\d{4}-\\d{2}-\\d{2}$")){
    stop("Date format incorrect; please enter in YYYY-MM-DD format.",
         call. = F)
  }



# -------------------------------------------------------------------------


# Detect number of coarse and fine particle diameters to include  --------

# this is an awfully ugly pipeline but having
# a hard time with the indexing. Despite its inelegance,
# it does get the job done

fines_diameters_sampled <- psa_protocols %>%
  tibble::enframe() %>%
  dplyr::filter(name == protocol_ID) %>%
  purrr::pluck("value") %>%
  .[[1]] %>%
  .[["fines_diameters_sampled"]] %>%
  .[[1]]

# copy-paste from above for the coarse-grained diameters

coarse_diameters_sampled <- psa_protocols %>%
  tibble::enframe() %>%
  dplyr::filter(name == protocol_ID) %>%
  purrr::pluck("value") %>%
  .[[1]] %>%
  .[["coarse_diameters_sampled"]] %>%
  .[[1]]



# -------------------------------------------------------------------------



# Sheets required for any type of test ------------------------------------

# browser()

psa_metadata <- tibble::tibble(
    date = date,
    experiment_name = experiment_name,
    sample_name = rep(sample_names, each= n_reps),
    replication = rep(1:n_reps, times = length(sample_names)),
    batch_sample_number = 1:(length(sample_names)*n_reps),
    protocol_ID = protocol_ID,
    comments = "-"
  )

  psa_specimen_masses_data <- tibble::tibble(
    date = date,
    experiment_name = experiment_name,
    sample_name = rep(sample_names, each = n_reps),
    replication = rep(1:n_reps, times = length(sample_names)),
    batch_sample_number = 1:(length(sample_names)*n_reps),
    air_dry_specimen_mass_for_test = "",
    comments = "-"
  )

  psa_hygroscopic_corrections_data <- tibble::tibble(
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




# For tests including pipette analysis ------------------------------------

  # determine which data sheets are needed for the fines method
  # assign them as objects

  # this should be done with `switch` but I am having a hard time wrapping
  # my mind around how to write the statement....
  # there are only 3 conditions, so for now I'll just use if statements
  # based on the data objects I constructed to match each protocol to
  # one of the three fines methods



  # Leaving off 2021-03-21 end of day

  # Where I am stuck is trying to not jam un-needed arguments
  # into this function - I want to stash them in another function (I think).
  # Either have to set them to NULL as defaults, use the dots to pass in args from other functions, or (as I now feel is most likely) need to create a "constructor function" that returns a list of arguments to pass along with the date, experiment name, etc. to the other functions inside this one which operate conditionally. So instead of pipette_sampling_datasheets() being a function that returns a list of data frames, maybe it should be a "constructor function" which instead returns a list of arguments, and these get passed to yet another function to generate the actual tibbles? Then this latter function would be called inside `psa_datasheets()`to generate the needed objects which are subsequently gathered with the call to `mget()` at the end.

  # check if pipette methods are used
  use_pipette <- protocol_ID %in% c(internal_data$pipette_invoking_protocol_IDs)

  # if pipette used, generate the relevant data sheets (one for the sampling, one for the blank measurements)
  if(use_pipette){

     # check that vector of beaker numbers (if supplied) is
    # a multiple of pipette sizes and number of reps and number of samples

# check if argument lengths are compatible

beaker_multiple <- length(sample_names) * n_reps * length(fines_diameters_sampled)


if((!is.null(pipette_beaker_numbers) && beaker_multiple != length(pipette_beaker_numbers))) {
        stop("Supplied beaker numbers vector is not a multiple of sample names x replications x number of particle diameters.",
             call. = F)
}

n_samples <- length(sample_names) * n_reps

if((!is.null(bouyoucos_cylinder_numbers) && length(bouyoucos_cylinder_numbers) != n_samples)) {
  stop("Length of vector `bouyoucos_cylinder_numbers` is not a multiple of number of sample names x replications.",
       call. = F)
}



pipette_sheets <- pipetting_datasheets()

# A very inelegant solution, but it works: rely on the 2 specific
      # return values of the internal call to pipette_sampling_datasheets
      # Then removes the originial list returned by that call
      # There has to be a better way with purrr::flatten or unlist
      # but these are not doing what I want and I have to move on

      psa_pipetting_data <- pipette_sheets$psa_pipetting_data

      psa_pipette_blank_correction_data <- pipette_sheets$psa_pipette_blank_correction_data

      # This step probably isn't necessary now that I figured out how to subset
      # the list of all existing objects based on being of class data frame instead
      # of list....but leaving in just to be sure
      rm(pipette_sheets)
 }




# for tests including hydrometer analysis ---------------------------------



 use_hydrometer <- protocol_ID %in% c(internal_data$hydrometer_invoking_protocol_IDs)
if(use_hydrometer){

  # generate the main datasheet and the sheet for the blank correction

 # browser()

  hydrometer_list <- hydrometer_datasheets()

  hydrometer_sheet_names <- names(hydrometer_list)

  # couldn't quite figure out how to use a functional
  # to do this (I think it's an issue with
  # it returning a new list containing the list element
  # rather than just the list element itself )

  # but a regular for loop seems to work fine

  for (i in seq_along(hydrometer_sheet_names)) {
    assign(x = hydrometer_sheet_names[[i]],
           value = hydrometer_list[[i]])
  }

  }




# for wash-through methods, i.e. total fines only -------------------------

# no additional sheets required beyond the datasheets common to all methods


# use_fines_laser_diffraction <- protocol_ID %in% c(fines_laser_diffraction_invoking_protocol_IDs)
  # if(use_fines_laser_diffraction){psa_fines_laser_diffraction_sampling_datasheets <- fines_laser_diffraction_sampling_datasheets(...)}



# -------------------------------------------------------------------------



  # for laser diffraction methods -------------------------------------------


  # have not yet built any functions to create data sheets or analyze laser diffraction data
  # it is likely nothing is needed here, as the data will be collected automatically
  # using Panalytical's software and then exported as a csv file

# For tests including sieving, either manually or with the Ro-tap ------------

# browser()

  use_sieves <- any(internal_data$sieve_invoking_protocol_IDs %in% protocol_ID)


  if(use_sieves){

    psa_sieving_data <- sieving_datasheet()

    }


use_pretreatment <- protocol_ID %in% c(internal_data$pretreatment_invoking_protocol_IDs)

if(use_pretreatment){

  psa_pretreatment_loss_data <- pretreatment_datasheet()

}



  # an elegant solution from the tidyverse and base r combined.....
  # compare to the long BS below. It is extra safe because it
# uses two ways to select: 1. a regular expression pattern following the naming convention
# I have adopted where the datasheet name begins with psa_ and ends with _data;
# the underscores are later converted to dashes for naming the files, and
# 2. the class attribute that it is a tibble. With both these in place I
# think this should be pretty bulletproof so long as any sheets I add
# at a later time are named this way

  all_datasheets <- mget(ls(pattern = "psa_.*data"), # leaving out the second underscore because the metadata sheet doesn't end in _data
                         mode = "list",
                      envir = rlang::current_env(),
                      inherits = F)
  # %>%
    # purrr::keep(tibble::is_tibble)


  # need something like this to keep only elements which have a data frame class:

  # I think this actually should be really simple with base R subsetting
  # but I just can't figure it out

  # I was so close with this attempt!! Just needed to iterate because all_opjbs is a list, then reduce the level because the [] subsetting expects an atomic vector instead ofa list
  # all_data_frames <- all_objs[is.data.frame(all_objs)]


# -------------------------------------------------------------------------



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

# all_data_frames <- all_sheets[vapply(X = all_objects,
 #                                    FUN = is.data.frame,
 #                                    FUN.VALUE = TRUE)]

  # An even more compact base R version which almost works but it's off by a level
  # all_datasheets <- mget(x = ls(sorted = FALSE), mode = "list")


  # An easier-to-write tidyverse solution with a typed map function !
  # all_data_frames <- all_objs[purrr::map_lgl(all_objs, is.data.frame)]

  # all_data_frames <- all_objs[as.logical(is.data.frame(all_objs))]


  # build paths to the new files to create

#   browser()

  basenames <- paste0(stringr::str_replace_all(names(all_datasheets), "_", "-"), "_", date, ".csv")

  file_paths_to_write <- here::here(
    new_folder,
    basenames)

  files_to_write <- tibble::tibble(
    x = all_datasheets,
    file = file_paths_to_write
  )


  # write the new folder and files to disk

  dir.create(path = new_folder)
  purrr::pwalk(.l = files_to_write, .f = readr::write_csv)

  n_succeeded <- sum(file.exists(file_paths_to_write))

  message(crayon::green(n_succeeded, " files were written to disk."))


}

