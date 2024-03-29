
#' Determine which functions to use for analyzing the data set
#'
#' An internal helper function for `psa()`
#'
#' @return Numeric vector of length 1
#'
find_protocol_ID <- function(){

dir <- get(x = "dir", envir = rlang::caller_env())

  # note that in this function the protocol is being read as a character column
  # to facilitate easy use of `switch()` later on

metadata_file <- readr::read_csv(
  list.files(
    path = dir, pattern = "metadata", full.names = T),
  col_types = "Dccciic", na = "-", lazy = FALSE)

  protocol_ID <- as.character(unique(metadata_file$protocol_ID))

  }




#' Divide file paths into common and method-specific
#'
#' Helper function for [`psa()`]. Returns a list of two character vectors - one
#' for the files common to all psa_protocols and one which has
#' the files unique to the protocol in use for this test
#'
#' @return List of length 2
#'
divide_psa_datafiles <- function(){

  # browser()

  # inherit the dir argument from parent call
  dir <- get(x = "dir", envir = rlang::caller_env())

  all_datafile_paths <-list.files(
    path = dir,
    pattern = "^psa-.*\\d{4}-\\d{2}-\\d{2}\\.csv$",
    full.names = T)

  # if user has supplied a directory in which no psa files exist,
  # halt function to give a better error message than R will on
  # its own
  if(length(all_datafile_paths) < 1){
    stop("No psa data files found in directory `", dir, "`. Did you supply the full path?", call. = FALSE)
  }


  datafile_names <- stringr::str_remove(
    string = basename(all_datafile_paths),
    pattern = "_\\d{4}-\\d{2}-\\d{2}\\.csv$") %>%
    stringr::str_remove_all("psa-")

  # over-write existing all_datafile_paths object with a named character vector

  all_datafile_paths <- purrr::set_names(all_datafile_paths, datafile_names)

  # find any file paths which match any of the patterns
  # this approach is not very elegant - it could definitely be done with
  # []. %in%, and/or grep or grepl.... however this works and I
  # can't waste any more time on it

  common_patterns <- tibble::tibble(
    pattern1 = "hygroscopic-corrections",
    pattern2 = "metadata",
    pattern3 = "specimen-masses",
    all_datafile_paths = all_datafile_paths
  )


  common_datafiles <- common_patterns %>%
    dplyr::filter(
      stringr::str_detect(all_datafile_paths, pattern = pattern1) |
        stringr::str_detect(all_datafile_paths, pattern = pattern2) |
        stringr::str_detect(all_datafile_paths, pattern = pattern3))%>%
    purrr::pluck("all_datafile_paths")

  method_specific_datafiles <- all_datafile_paths[!all_datafile_paths %in% common_datafiles]

  return_list <- mget(ls(pattern = "_datafiles"), envir = rlang::current_env())

  return(return_list)

}


#' Clean up a vector of file paths and then import each one as a tibble
#'
#' Assign the name while also making sure the protocol ID is imported
#' as a character type
#'
#' @param x character vector of file paths
#'
#' @return list of data frames
#'
import_psa_datafile <- function(x){

  #  browser()

  nm <- tools::file_path_sans_ext(basename(x)) %>%
    stringr::str_remove(pattern = c("^psa-") ) %>%
    stringr::str_remove(pattern = c("_\\d{4}-\\d{2}-\\d{2}") ) %>%
    stringr::str_remove(pattern = c("-data$") ) %>%
    stringr::str_replace_all("-", "_")

  # this is a little complex and maybe I could have found a better way to do
  # it, but I finally succeeded in using modify_if.....for each list element
  # (in this particular case there are only 2 but it would generalize),
  # this checks if the data frame has a column named "protocol_ID" and then
  # if it does it applies my anonymous function to mutate that column
  # into a charcter type. In this way the rest of the data frames are "shielded"
  # from that function; i.e. they will never see it which allows me to use the
  # normal NSE syntax for the mutate call

  # This is a very useful technique and one I should return to for other similar
  # problems.
  # For example, I use the same method to easily modify any data frames that contain
  # the tin tare set or beaker tare set as a character type
  # to have a character type, because this will otherwise mess up joining
  # later in the process. Yea, this is DOPE!

  # Finally I use readr::parse_number to remove the "g" sometimes
  # printed by electronic balances. Sometimes the columns with these
  # get read in as numeric (if there is no g printed), so I have to
  # coerce the relevant columns to character types and _then_ parse the
  # number

  # browser()

return_dfs <-   x %>%
    purrr::set_names(nm) %>%
    purrr::map(readr::read_csv,
               show_col_types = FALSE,
               na = "-",
               trim_ws = FALSE,
               skip_empty_rows = TRUE,
               lazy = FALSE) %>%
    purrr::modify_if(.p = ~any(names(.) %in% "protocol_ID"),
                     .f = ~dplyr::mutate(. , protocol_ID = as.character(protocol_ID))) %>%
    purrr::modify_if(.p = ~any(names(.) %in% "beaker_tare_set"),
                     .f = ~dplyr::mutate(. , beaker_tare_set = as.character(beaker_tare_set))) %>%
    purrr::modify_if(.p = ~any(names(.) %in% "tin_tare_set"),
                     .f = ~dplyr::mutate(. , tin_tare_set = as.character(tin_tare_set))) %>%
    purrr::modify_if(
      .p = ~any(
        stringr::str_detect(
          string = names(.),
          pattern = "^tin_w_.*sample")),
      .f = ~dplyr::mutate(. ,
                          dplyr::across(
                            .cols = dplyr::matches(
                              match = "^tin_w_\\w*_sample$"),
                            .fns = ~readr::parse_number(as.character(.)))))


return(return_dfs)

  }



#' Determine loss of mass during specimen pretreatment
#'
#' Rather than oven-drying the pretreated sample and then re-dispersing it,
#' it is often more convenient to run a separate (duplicate) analysis of the
#' pretreatment for organic matter, carbonates, or Fe-oxides and subtract the
#' pretreatement loss from the actual tested specimen's initial oven-dry mass.
#' `compute_preatreatment_loss()` does this by reading a data file titled
#' 'pretreatment_loss_data_\<date\>.csv` and referring to the other files containing
#' the air-dry specimen masses and the hygrscopic water contents.
#'
#' @return tibble of sample info plus % of OD sample mass lost
#'
compute_pretreatment_loss <- function(){

# assign the required objects to the current function environment from the parent caller

  required_objs <- mget(x = c("dir", "hygroscopic_water_contents", "method_specific_datafiles"),
       envir = rlang::caller_env())

  list2env(required_objs, envir = rlang::current_env())

  # browser()

pretreatment_loss_pcts <- method_specific_datafiles$pretreatment_loss %>%
  dplyr::left_join(hygroscopic_water_contents,
                   by = c("date", "experiment_name", "sample_name", "replication", "batch_sample_number")) %>%
    dplyr::mutate(OD_soil_mass_before_pretreatment = .data$air_dry_specimen_mass_before_pretreatment / (1 + .data$hygroscopic_water_content),
           OD_soil_mass_after_pretreatment = .data$container_mass_w_OD_sample - .data$container_tare,
           pretreatment_loss_mass = .data$OD_soil_mass_before_pretreatment - .data$OD_soil_mass_after_pretreatment,
           pretreatment_loss_pct = .data$pretreatment_loss_mass / .data$OD_soil_mass_before_pretreatment) %>%
    dplyr::select(.data$date:.data$batch_sample_number, .data$pretreatment_loss_pct)

  return(pretreatment_loss_pcts)
}



#' Checks protocol list to see if OD specimen mass must be adjusted
#'
#' @return Logical value of length 1
#'
check_pretreatment_correction <- function(){

  protocol_ID <- get(x = "protocol_ID", envir = rlang::caller_env())

  # return a logical based on a match with the internal data object containing
  # the methods that use pretreatment

  protocol_ID %in% internal_data$pretreatment_invoking_protocol_IDs

  }




#' Calculate % finer than an arbitrary number of pipette sizes
#'
#' Computes blank correction, subtracts from each beaker, then divides beaker
#' contents by overall OD specimen mass
#'
#' @return data frame named `fines_pct_passing`
#'
compute_pipette_fines_pct_passing <- function(with_hydrometer = FALSE){

  # inherit the existing objects needed for computation
  # from the parent function environment

  # when called inside another function which uses data for _both_ the hydrometer and pipette, this call needs to look one level higher in the call stack, hence the conditional statement
  # for the environment

#  browser()

needed_objs <- mget(x = c("method_specific_datafiles", "OD_specimen_masses", "beaker_tares", "coarse_percent_passing", "protocol_ID"),
                    envir = rlang::caller_env(n = dplyr::if_else(with_hydrometer, 2, 1)))

# make them available in the current function call
list2env(needed_objs,envir = rlang::current_env())

# find beaker tares through user-supplied argument or option
beaker_tares <- beaker_tares %||% getOption('soiltestr.beaker_tares') %||% internal_data$equipment_instructions("beaker_tares")


# locate beaker tare set from data files

  # compute blank correction

  # browser()

blanks_df <- method_specific_datafiles$pipette_blank_correction %>%
    dplyr::left_join(beaker_tares, by = c("beaker_number", "beaker_tare_set")) %>%
    dplyr::mutate(calgon_in_beaker = .data$beaker_mass_w_OD_sample - .data$beaker_empty_mass)

blank_correction <- mean(blanks_df$calgon_in_beaker, na.rm = TRUE)

  # calculate % passing for each size


# if the > 53 micron fraction was not washed through the 270
# sieve prior to pipetting, the volume of liquid is less than
# 1000 mL. Compute the actual volume of liquid and use as a
# multiplier to the soil contained in the beaker
# assume a specific gravity of 2.65 g/cm3. If the sample was washed
# through the 270 before pipetting, just multiply the sample mass in
# the beaker by 40


# browser()

if(protocol_ID %in% internal_data$after_fines_sampling_wash_through_protocol_IDs){


  mass_multipliers <- coarse_percent_passing %>%
    dplyr::group_by(.data$sample_name, .data$batch_sample_number) %>%
    dplyr::filter(dplyr::near(.data$microns, 53)) %>%
    dplyr::summarise(sand_plus_gravel = 1 - .data$percent_passing) %>%
    dplyr::left_join(
      OD_specimen_masses,
      by = c("sample_name",
             "batch_sample_number")) %>%
    dplyr::mutate(
      coarse_particles_mass = sand_plus_gravel * OD_specimen_mass,
      coarse_particles_volume = coarse_particles_mass / 2.7,
      mass_multiplier = (1000 - coarse_particles_volume) / 25
    ) %>%
    dplyr::select(sample_name, batch_sample_number, mass_multiplier)

} else{
  # here a table of the same dimensions is made, but the multipliers are
  # all 40, which is a 25 mL pipette sample scaled to 1000 mL
  mass_multipliers <-  coarse_percent_passing %>%
    dplyr::distinct(sample_name, batch_sample_number) %>%
    dplyr::mutate(mass_multiplier = 40)
  }


  fines_percent_passing <- method_specific_datafiles$pipetting %>%
    dplyr::left_join(
      beaker_tares,
      by = c("beaker_number", "beaker_tare_set")) %>%
    dplyr::left_join(
      OD_specimen_masses,
      by = c("date", "experiment_name",
             "sample_name", "replication", "batch_sample_number")) %>%
    dplyr::left_join(
      mass_multipliers,
      by = c("sample_name","batch_sample_number")) %>%
    dplyr::mutate(
      total_g_in_beaker = .data$beaker_mass_w_OD_sample - .data$beaker_empty_mass,
      soil_in_beaker = .data$total_g_in_beaker - blank_correction,
      percent_passing = (mass_multiplier * .data$soil_in_beaker) / .data$OD_specimen_mass) %>%
    dplyr::select(.data$date:.data$batch_sample_number, .data$microns, percent_passing)







  return(fines_percent_passing)
}

# the function compute_152H_hydrometer_fines_pct_passing() is kept in
# its own file. It needs a number of helpers which are specific to that
# function, and it is getting pretty long. Pulling out so it is easier to work through and so it does not clutter this file so badly.

# Like `compute_pipette_fines_pct_passing()`, the
# `compute_152H_hydrometer_fines_pct_passing()` function
# also returns a data frame called `fines_percent_passing()`
# with an arbitrary number of particle diameters:



# this one is a helper for exiting early when user only is doing a
# wash-though analysis to compute total coarse content and total fines
# (i.e. no separation between silt and clay)

#' Compute gravel, sand, and total fines
#'
#' (Internal)
#'
#' @return tibble with one row per specimen
#'
wash_through_coarse_grains <- function(){

  # browser()

  # find simple bins data frame

  cumulative_percent_passing <- get("cumulative_percent_passing", envir = rlang::caller_env() )

  wash_through_size_bins <- cumulative_percent_passing %>%
    dplyr::filter(!is.na(microns)) %>%
    tidyr::pivot_wider(names_from = .data$microns,
                       values_from = .data$percent_passing) %>%
    dplyr::mutate(
      gravel = .data$`4000` - .data$`2000`,
      sand = .data$`2000` - .data$`53`,
      fines = .data$`53` ) %>%
    dplyr::select(.data$date:.data$batch_sample_number,
                  .data$gravel:.data$fines) %>%
    dplyr::mutate(
      dplyr::across(
        .cols = .data$gravel:.data$fines,
        .fns = ~.*100))

  return(wash_through_size_bins)

}


#' Calculate % finer for arbitrary number of sieves
#'
#' @return data frame
#'
compute_sieves_percent_passing <- function(){

  # find required objects from calling environment

  # browser()

  needed_objs <- mget(x = c("method_specific_datafiles", "OD_specimen_masses"),
                      envir = rlang::caller_env())

  # make them available in the current function call
  list2env(needed_objs,envir = rlang::current_env())


    sieves_percent_passing <- method_specific_datafiles$sieving %>%
      dplyr::left_join(
        OD_specimen_masses,
        by = c("date", "experiment_name", "sample_name", "replication", "batch_sample_number"))%>%
      dplyr::mutate(cumulative_mass_finer = .data$OD_specimen_mass - .data$cumulative_mass_g,
                    percent_passing = .data$cumulative_mass_finer / .data$OD_specimen_mass) %>%
    dplyr::select(.data$date:.data$batch_sample_number, .data$microns, percent_passing)

    return(sieves_percent_passing)
  }


#' Wrangle a csv of mastersizer data
#'
#' @param ...
#'
#' @return data frame containing metadata plus cumulative percent passing
#'
compute_mastersizer_fines_pct_passing <- function(...){

  # function written 2022-04-15
  # Will need to test it out once the sand is sieved for the samples
  # tested today @ MCL



  # find needed objects
  needed_objs <- mget(x = c("method_specific_datafiles", "OD_specimen_masses", "coarse_percent_passing", "protocol_ID"),
                      envir = rlang::caller_env())

  # make them available in the current function call
  list2env(needed_objs,envir = rlang::current_env())




  raw_fines_df <- method_specific_datafiles$mastersizer %>%
    dplyr::mutate(cumulative_percent_passing_normalized_to_total_fines = readr::parse_number(cumulative_percent_passing_normalized_to_total_fines))

  total_fines_df <- coarse_percent_passing %>%
    dplyr::group_by(batch_sample_number) %>%
    dplyr::summarise(total_fines = min(percent_passing))

 #  browser()

  fines_df <- total_fines_df %>%
    dplyr::left_join(raw_fines_df, by =  'batch_sample_number') %>%
    dplyr::mutate(
      percent_passing = cumulative_percent_passing_normalized_to_total_fines * total_fines
    ) %>%
    dplyr::select(c(batch_sample_number, microns, percent_passing)) %>%
    dplyr::filter(microns < 53) # to eliminate data points not collected w/ mastersizer

  # now just need to interpolate the desired fines diameters
  # from the mastersizer data

  # instead of using the function I wrote for doing the same thing on hydrometer data,
  # make a new version here. The other one is too complicated because it also does
  # hydrometer-specific calculations.



  fines_df_w_predicted_values <- compute_percent_passing_x_microns(x = fines_df, desired_diameters = c(20, 5, 2, 0.2))

return(fines_df_w_predicted_values)



}




#' calculate the percent finer than a given particle diameter
#'
#' @param x data frame containing columns `microns` and `percent_passing`
#' @param desired_diameters numeric vector of diameters on which to to compute
#'
#' @return data frame with new values added for the desired particle diameters
compute_percent_passing_x_microns <- function(x, desired_diameters){

  distances_dfs <- vector("list", length = length(desired_diameters))


  for(i in seq_along(desired_diameters)){

  temp_dfs <- x %>%
      split(~batch_sample_number) %>%
      purrr::map(~dplyr::mutate(., dist_from_desired  = microns  - desired_diameters[[i]]))

find_min_pos_dist <-  function(x){

  min(x$dist_from_desired[x$dist_from_desired > 0])

}

find_min_neg_dist <-  function(x){

  min(abs(x$dist_from_desired[x$dist_from_desired < 0]))

      }

min_pos_dist <- unique(purrr::map_dbl(temp_dfs, find_min_pos_dist))

min_neg_dist <- unique(purrr::map_dbl(temp_dfs, find_min_neg_dist))


# distances_dfs[[i]] <- temp_dfs %>%
#       purrr::map(., ~dplyr::filter(dplyr::near(microns, min_pos_dists[[i]]) | dplyr::near(microns, -min_neg_dists[[i]])))


distances_dfs[[i]] <- temp_dfs %>%
  dplyr::bind_rows() %>%
  dplyr::filter(
    dplyr::near(dist_from_desired, min_pos_dist) | dplyr::near(dist_from_desired, -min_neg_dist))


  }

  # fit the log-linear model

  # browser()

mods <-  vector("list", length = length(desired_diameters))


for(i in seq_along(mods)){

  mods[[i]] <- distances_dfs[[i]] %>%
    split(~batch_sample_number) %>%
    purrr::map(~lm(data = ., formula = percent_passing ~ log10(microns)  ))


}

  # predict the percent passing for the requested particle diameter

# browser()

predicted_values <- mods %>%
  tibble::enframe(value = "model") %>%
  dplyr::mutate(microns = desired_diameters,
                batch_sample_number = list(1:length(temp_dfs))) %>%
  tidyr::unnest(cols = c(model, batch_sample_number)) %>%
  dplyr::mutate(
    microns_df = purrr::map(microns, ~tibble::tibble(microns = .)),
    percent_passing  = mapply(
      object = model,
      newdata = microns_df, FUN= predict)
  ) %>%
  dplyr::select(
    batch_sample_number, microns, percent_passing
  )

  # predicted_percent_passing <- predict(
  #   object = log_lin_mod,
  #   newdata = data.frame(D_m_microns = d_microns)) %>%
  #   purrr::set_names(paste0('< ', d_microns, '\u03bcm'))

return_df <- dplyr::bind_rows(
  x, predicted_values
) %>%
  dplyr::arrange(batch_sample_number, dplyr::desc(microns))

}






#############################





#this finds the difference in microns between the closest size actually
#measured and the requested particle diameter. The min_pos_dist value is the
#distance of the diameter slightly larger than the one requested, and the
#min_neg_dist is the distance which is slightly smaller than the one
#requested.



  # filter the data frame to contain only the desired data points....discern these
  # by assigning two local variables, one for the smallest distance above the
  # desired diameter and one for the smallest distance above it

  # min_pos_dist <- min(purrr::pluck(distances_df[distances_df$diff_from_desired_diameter > 0, ,], "diff_from_desired_diameter"))

  # min_neg_dist <- min(abs(purrr::pluck(distances_df[distances_df$diff_from_desired_diameter <= 0, ,], "diff_from_desired_diameter")))

  # those values can now be used to filter the main data frame to include only
  # rows having those values for the diff_from_desired_diameter variable

  # filtered_distances_df <- distances_df %>%
  #   dplyr::filter(
  #     dplyr::near(diff_from_desired_diameter, min_pos_dist) | dplyr::near(diff_from_desired_diameter, -min_neg_dist))




  ################################




#' Internal helper to determine whether mastersizer data need to be wrangled
#' into a single file
#'
#' @return logical of length 1
#'
detect_mastersizer_csv <- function(){

  # browser()

  dir <- get("dir", rlang::caller_env())

  n_mastersizer_files <- list.files(path = dir, pattern = "mastersizer.*\\.csv$", full.names = T, recursive = F) %>%
    length()

  csv_exists <- n_mastersizer_files  > 0

  return(csv_exists)


}

