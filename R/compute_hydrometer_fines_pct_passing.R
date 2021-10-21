compute_152H_hydrometer_fines_pct_passing <- function(with_pipette = FALSE){

  # browser()

  # find needed objects from caller environment and
  # make them available in the current function call

  # if this function is being called in a protocol
  # which uses _both_ the hydrometer and pipette,
  # it will need to look one more level up in the call stack,
  # hence the conditional statement inside rlang::caller_env()

  needed_objs <- mget(x = c("method_specific_datafiles",
                            "OD_specimen_masses",
                            "tin_tares",
                            "bouyoucos_cylinder_dims",
                            "hydrometer_dims"),
                      envir = rlang::caller_env(n = dplyr::if_else(with_pipette, 2, 1)))
  list2env(needed_objs, envir = rlang::current_env())


  # assign bouyoucos cylinder metadata as its own data frame
  # check if user has supplied a data frame and if not, look for a global option...
  # if none found, throw an error.

  bouyoucos_cylinder_dims <- bouyoucos_cylinder_dims %||% getOption('soiltestr.bouyoucos_cylinder_dims') %||% internal_data$equipment_instructions("bouyoucos_cylinder_dims")

  # tin tares have already been imported from global environment,
  # so no need to repeat here

  # get hydrometer dimensions from option or initial function call

  hydrometer_dims <- hydrometer_dims %||% getOption('soiltestr.hydrometer_dims') %||% internal_data$equipment_instructions("hydrometer_dims")

  rho_water_152H <- h2o_properties_w_temp_c[h2o_properties_w_temp_c$water_temp_c == 20L, "water_density_Mg_m3"][[1]]

  # calculate % passing for each size ---------------------------------------



  # first rename the data frames; this function is rather long and
  # it is easier to refer to them inside this
  # function environment if they are not part of a list....plus
  # need to make the names easier to differentiate; for example the name
  # hydrometer reading is used for both the blank correction data sheet
  # and the actual hydrometer data sheet. Easier to keep it that way for
  # internal consistency and just make the single adjustment here

   blank_correction_data <-  method_specific_datafiles$hydrometer_blank_correction_w_companion %>%
    dplyr::rename(blank_hydrometer_reading = hydrometer_reading) %>%
    dplyr::left_join(hydrometer_dims, by = "hydrometer_ID") %>%
    dplyr::select(.data$date,
                  .data$experiment_name,
                  .data$approx_ESD,
                  .data$blank_hydrometer_reading)

  # join hydrometer data with the other required information,
  # then create datetime objects for stir time and
  # sampling time from the appropriate components. Finally,
  # join with specimen masses

  browser()

  hydrometer_data <- method_specific_datafiles$hydrometer %>%
    dplyr::mutate(approx_ESD = as.numeric(approx_ESD)) %>%
    dplyr::left_join(
      blank_correction_data, by = c("date", "experiment_name", "approx_ESD")
    ) %>%
    dplyr::left_join(bouyoucos_cylinder_dims, by = "bouyoucos_cylinder_number") %>%
    dplyr::left_join(h2o_properties_w_temp_c, by = "water_temp_c") %>%
    tidyr::unite(col = 'sampling_datetime',
                 sampling_date,
                 sampling_time,
                 sampling_AM_PM,
                 sep = ' ') %>%
    tidyr::unite(col = 'stir_datetime',
                 stir_date,
                 stir_time,
                 stir_AM_PM,
                 sep = ' ') %>%
    dplyr::mutate(
      sampling_datetime = dplyr::if_else(
        stringr::str_detect(
          sampling_datetime,
          pattern = "\\d{1,2}:\\d{2}:\\d{2}\\s+(A|P)M$"),
        lubridate::ymd_hms(sampling_datetime, tz = Sys.timezone()),
        lubridate::ymd_hm(sampling_datetime, tz = Sys.timezone())
        ),
      stir_datetime = dplyr::if_else(
        stringr::str_detect(
          stir_datetime,
          pattern = "\\d{1,2}:\\d{2}:\\d{2}\\s+(A|P)M$"),
        lubridate::ymd_hms(stir_datetime, tz = Sys.timezone()),
        lubridate::ymd_hm(stir_datetime, tz = Sys.timezone())
      ),
      elapsed_time = lubridate::as.duration(sampling_datetime - stir_datetime)) %>%
    dplyr::left_join(OD_specimen_masses, by = c("date", "experiment_name", "sample_name", "replication", "batch_sample_number"))

  # get hydrometer used for test
  hydrometer_ID <- unique(hydrometer_data$hydrometer_ID)


 # particle calculations ---------------------------------------------------

  # these are pretty wicked, they have a lot of nested parentheses
  # so I have divided them into multiple steps, one for each calculation
  # as outlined in ASTM D7928-17


  # this is the point where the data frame needs to be split into
  # multiple data frames by sample and then each calculation applied
  # before running the generalized_finer_D_x()
  # function on each data frame in the list column.


   # browser()

calculations_dfs <- hydrometer_data %>%
    dplyr::group_by(batch_sample_number) %>%
    dplyr::group_split() %>%
    purrr::map(hydrometer_calcs_part_1)

    # not nesting any more, back to just a regular piped map
#    tidyr::nest() %>%
 #   dplyr::mutate(
  #    calculation_df = purrr::map(data, hydrometer_calcs_part_1))


  # percent passing for a given size ----------------------------------------

  # now need to do the more complex part of computing the actual %
  # finer than a diameter requested by the user
  # it requires a log-linear interpolation to calculate each %

  # the helper functions to do this appear later in this file

 #  test_df <- calculations_dfs[[1]]

  # it works for one sample like so:
  # generalized_finer_D_x(calculations_df = test_df ,
  #                       d_microns = 2)

# try to run it for the whole list, then rbind all the data frames together

 # browser()
all_results <- purrr::map(calculations_dfs, generalized_finer_D_x, with_pipette = with_pipette)

fines_percent_passing <- dplyr::bind_rows(all_results) %>%
  dplyr::arrange(.data$batch_sample_number,
                 dplyr::desc(.data$microns))

return(fines_percent_passing)

# fines_percent_passing

}


# some helpers and wrappers for `compute_hydrometer_fines_pct_passing --------



# hydrometer calcs 1-----------------------------------------------------------

# from sections 12.4-12.6 in ASTM D7928-17

#the calculations based on the measured hydrometer values, NOT the curve interpolation (that is done with separate functions defined below)

#' Calculates various parameters needed to construct the particle size curve
#'
#' Results passed to other functions which interpolate along the curve
#' to obtain desired % passing values
#'
#' @param x a data frame containing relevant columns, these are produced by the
#'   actions inside `compute_hydromter_fines_pct_passing()`  _before_ this function
#'   is called.
#'
#' @return
#'
hydrometer_calcs_part_1 <- function(x){



#   browser()

  # assign the required constants which exist in higher frames
  hydrometer_dims <- get(x = c("hydrometer_dims"), envir = rlang::caller_env(2))

mass_percent_finer_df <- x %>%
  dplyr::filter(!is.na(hydrometer_reading)) %>%
  dplyr::mutate(
    mass_pct_finer = 0.6226 * (Gs / (Gs - 1)) * (1000 / OD_specimen_mass) * (hydrometer_reading - blank_hydrometer_reading) * (100 / 1000)
  )

effective_depth_df <- mass_percent_finer_df %>%
  dplyr::mutate(Hm_cm = hydrometer_dims$Hr2_cm + (
    (hydrometer_dims$Hr1_cm - hydrometer_dims$Hr2_cm) / ((60) - (-5)) * (60 - hydrometer_reading + meniscus_correction)
  ) - (hydrometer_dims$Vhb_mL / (2 * area_cm2)))

max_diam_in_suspension_df <- effective_depth_df %>%
  dplyr::mutate(
    root_term1 = (18 * water_absolute_viscosity_poises) / (water_density_Mg_m3 * 980.7 * (Gs -
                                                                                            1)),
    root_term_2 = (Hm_cm / as.double(elapsed_time)),
    root_term_total = sqrt(root_term1 * root_term_2),
    D_m_mm = root_term_total * 10,
    D_m_microns = D_m_mm * 1000
  )


return_df <- max_diam_in_suspension_df %>%
  dplyr::select(.data$date:.data$batch_sample_number,
                .data$mass_pct_finer,
                .data$D_m_microns)

return(return_df)

}

# percent_passing for one data point --------------------------------------


# the function which actually calculates the % finer for a particular single test data point.

#' Calculate % finer than a given diameter for a single sample
#'
#'
#' @param d_microns the ESD particle diameter for which % finer is being computed.
#' This is not passed as an argument in the function call but instead is found in the caller environment (which is the wrapper to loop over any number of particle diameters). I could have just defined this function inside the other one, but it was getting pretty long so I decided to break this outside of its wrapper.
#'
#' @return data frame containing the original metadata along with the micron value of interest and the corresponding percent passing...for a single particle diameter
#'
#' @details Finds two particle diameters which bracket that number
#' and then fits a model as `percent_passing ~ log10(microns)`.
#' Lastly it predicts the value for the specified diameter using
#' that model object.
#'
#' This function is then wrapped by another which generalizes it to
#' a d_microns vector of any length, so long as the values fall between
#' those measured during the hydrometer test
#'
percent_finer_D_x <- function(calculations_df, d_microns) {

 # find and assign the required arguments from the caller environment, which
  # is the function environment of the generalized wrapper
  # around this function. The arguments have the same names.

  # old approach before I was nesting early
  # list2env(
  #   mget(c("max_diam_in_suspension_df", "d_microns"), envir = rlang::caller_env()),
  #   envir = rlang::current_env())

  # calculations_df <- get("calculations_df", envir = rlang::caller_env())

  # list2env(
  #   mget(c("d_microns"), envir = rlang::caller_env()),
  #   envir = rlang::current_env())



  # browser()

  # calculate differences for all sampled diameters from the
  # specified diameter
  # old approach
  #distances_df <- max_diam_in_suspension_df %>%
   # dplyr::mutate(diff_from_desired_diameter = D_m_microns - d_microns)

  distances_df <- calculations_df %>%
    dplyr::mutate(diff_from_desired_diameter = D_m_microns - d_microns)




  #this finds the difference in microns between the closest size actually
  #measured and the requested particle diameter. The min_pos_dist value is the
  #distance of the diameter slightly larger than the one requested, and the
  #min_neg_dist is the distance which is slightly smaller than the one
  #requested.


  if(all(distances_df$diff_from_desired_diameter < 0) | !any(distances_df$diff_from_desired_diameter < 0)){

    warning("Sample \'", unique(calculations_df$sample_name), "\', replication ", unique(calculations_df$replication), ": unable to filter for two particle diameters which bracket requested diameter of ", d_microns, " \u03bcm; inserting NA value.",
            call. = FALSE)

    predicted_percent_passing <- NA %>%
      purrr::set_names(paste0('< ', d_microns, '\u03bcm'))

  } else{


    # this is the desired scenario
    # filter the data frame to contain only the desired data points....discern these
    # by assigning two local variables, one for the smallest distance above the
    # desired diameter and one for the smallest distance above it

    min_pos_dist <- min(purrr::pluck(distances_df[distances_df$diff_from_desired_diameter > 0, ,], "diff_from_desired_diameter"))

    min_neg_dist <- min(abs(purrr::pluck(distances_df[distances_df$diff_from_desired_diameter <= 0, ,], "diff_from_desired_diameter")))

    # those values can now be used to filter the main data frame to include only
    # rows having those values for the diff_from_desired_diameter variable

    filtered_distances_df <- distances_df %>%
      dplyr::filter(
        dplyr::near(diff_from_desired_diameter, min_pos_dist) | dplyr::near(diff_from_desired_diameter, -min_neg_dist))


    # fit the log-linear model

    log_lin_mod <- lm(
      data = filtered_distances_df,
      formula = mass_pct_finer ~ log10(D_m_microns)
    )

    # predict the percent passing for the requested particle diameter

    predicted_percent_passing <- predict(
      object = log_lin_mod,
      newdata = data.frame(D_m_microns = d_microns)) %>%
      purrr::set_names(paste0('< ', d_microns, '\u03bcm'))

}

  # add the predicted value onto the original data frame,
  # select only the desired columns,
  # and keep only distinct rows (there should be only two rows, but
  # because the predicted percent passing is simply recycled for both of them,
  # all the relevant metadata values should be identical)
  # Finally, convert all columns containing percent passing to decimal form
  # as they are currently in whole-number percentage form

 # browser()

  return_df <- calculations_df %>%
    dplyr::mutate(
      "percent_passing_{d_microns}_microns" := predicted_percent_passing) %>%
    dplyr::select(
      .data$date,
      .data$experiment_name,
      .data$protocol_ID,
      .data$sample_name,
      .data$replication,
      .data$batch_sample_number,
      dplyr::matches("^percent_passing_\\d.*_microns$")
    ) %>%
    dplyr::distinct() %>%
    dplyr::mutate(
      dplyr::across(
        .cols = dplyr::matches("^percent_passing_\\d.*_microns$"),
        .fns = ~. * 0.01
      )
    )

  return(return_df)

  # end of function to predict a single diameter

}



# wrapper for multiple data points ----------------------------------------


# a wrapper which generalizes the above to any number of particle diameters

#' Iterate over multiple particle diameters for a given sample
#'
#' @param d_microns Numeric vector of arbitrary length, corresponding to the particle diameters to compute. Defaults to 2, which is the silt/clay cutoff.
#' @param with_pipette Logical, whether function is being called from inside another function, necessitating that the protocol ID is found one more level up in the call stack
#'
#' @return data frame in long format with microns and percent_passing as columns
#'
generalized_finer_D_x <- function(calculations_df = NULL, d_microns = NULL, with_pipette = FALSE){

  # instead of passing in a data frame as an argument to this function,
  # I am going to use the
  # same strategy used elsewhere in this package...
  # find the required objects from the caller environment

 # browser()


  # determine what the fines diameters to compute are, and
  # assign as a variable for d_microns....the default is NULL,
  # and in the next step below I pass this vector to the percent_finer_D_x
  # function along with the original data frame. I initially tried
  # this with a loop but I like pmap better, it is more concise
  # and feels safer since it is a named list

  # to find the fines diameters sampled, only need to determine which
  # protocol is being used and then look it up in the psa_protocols
  # list, as this contains a vector with this exact piece of information

  # browser()

  protocol_ID <- get("protocol_ID", envir = rlang::caller_env(n = dplyr::if_else(with_pipette, 4, 3)))
  d_microns <-psa_protocols[[rlang::sym(protocol_ID)]][["fines_diameters_to_compute"]][[1]]

  # throws an error if there are no diameters to compute
  if(is.null(d_microns) | length(d_microns) < 1){
    stop("No fines diameters found to compute for protocol_ID", protocol_ID,
         call. = FALSE)
  }

  # having trouble with map2, try pmap instead which takes a named list


  # I broke protocol 8, but I think it is fixable pretty easily

  # filter for any diameters finer than 1 micron, as they can't be computed via sedimentation.
  # THe only reason these would exist is when doing a dual hydrometer/pipette method....this way the arguments can stay the same but there is no attempt to compute a diameter for anything that is too fine

  # not sure what is up wit this line of code....could have been a mis-placed copy/paste?? dfs_list <- parallel_args <-  tibble::tibble(

  browser()
  dfs_list <- tibble::tibble(
    calculations_df = purrr::rerun(.n = length(d_microns),
                                   calculations_df),
    d_microns = d_microns
  ) %>%
    dplyr::filter(d_microns > 1) %>%
    purrr::pmap(percent_finer_D_x)


  # if there is more than one diameter to compute, reduce the data frames by
  # left joining...this leaves one new column per diameter. If there is only
  # one diameter, just flatten it from a list into a single data frame

 #  browser()

  if(length(d_microns) == 1L){
    reduced_predicted_sizes <- purrr::flatten_df(dfs_list)
  } else {
    reduced_predicted_sizes <- purrr::reduce(dfs_list, dplyr::left_join,
                                         by = c("date",
                                                "experiment_name",
                                                "protocol_ID",
                                                "sample_name",
                                                "replication",
                                                "batch_sample_number"))
  }


  # pick out only the relevant columns and put back into long format

  all_sizes_predicted <- reduced_predicted_sizes %>%
    dplyr::select(.data$date,
                  .data$experiment_name,
                  .data$protocol_ID,
                  .data$sample_name,
                  .data$replication,
                  .data$batch_sample_number,
                  dplyr::matches(
                    "^percent_passing_\\d.*_microns$"
                  )) %>%
    tidyr::pivot_longer(
      cols =  dplyr::matches("^percent_passing_\\d.*_microns$"),
      names_to = 'microns',
      values_to = 'percent_passing'
    ) %>%
    dplyr::mutate(microns = readr::parse_number(.data$microns)
    ) %>%
    dplyr::arrange(dplyr::desc(microns))


  return(all_sizes_predicted)

  }
