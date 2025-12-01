
# checks for whether complex bins can be computed  ------------------------

check_for_fines_complex_bins <- function(){

  protocol_ID <- get("protocol_ID", envir = rlang::caller_env())

  # return a logical based on whether at least 3 particle diameters were sampled
  # for the fines method
  protocol_ID %in% internal_data$fines_sub_bin_invoking_protocol_IDs

}



check_for_coarse_complex_bins <- function(){

  protocol_ID <- get("protocol_ID", envir = rlang::caller_env())

  # return a logical based on whether at least 3 particle diameters were sampled
  # for the coarse method

  protocol_ID %in% internal_data$coarse_sub_bin_invoking_protocol_IDs

}


# helpers for generating/cleaning the data frames created by the o --------

#' (Internal)
#'
#' Helper for generating whole number from decimals
#' when computing size bins
#'
#' @param df data frame
#'
#' @return same data frame with any columns whose names begin with a
#' number multiplied by 100
#'
psa_decimal_to_pct <- function(df){

  df %>%
    dplyr::mutate(
      dplyr::across(
        .cols = dplyr::matches("^\\d+"),
        .fns = ~.*100))

}


#' (Internal)
#'
#' Helper for selecting all columns that don't have names beginning with a number
#'
#' @param df data frame
#'
#' @return same data frame with relevant columns removed
#'
psa_remove_number_bins <- function(df){

  df %>%
    dplyr::select(-c(dplyr::matches("^\\d+")))

}

# methods for coarse size bins ---------------------------------------------

#' (Internal)
#'
#' Size bins for USGA sieves
#'
#'
#' @return a tibble
#'
USGA_bins <- function(){

 #  browser()

  # assign cumulative percent passing to local variable based on its value in global environment

  cumulative_percent_passing <- get("cumulative_percent_passing", envir = rlang::caller_env() )

  usga_bins <- cumulative_percent_passing %>%
    dplyr::filter(microns >= 53) %>%
    tidyr::pivot_wider(names_from = microns, values_from = percent_passing) %>%
    psa_decimal_to_pct() %>%
    dplyr::mutate(
      gravel = `4000` - `2000`,
      very_coarse_sand = `2000` - `1000`,
      coarse_sand = `1000` - `500`,
      medium_sand = `500` - `250`,
      fine_sand = `250` - `150`,
      very_fine_sand = `150` - `53`) %>%
    psa_remove_number_bins()


  return(usga_bins)

}


#' Same as usga_bins() but with more gravel sieves
#'
#' @return a tibble
#'
USGA_bins_plus_gravel <- function(){

  #  browser()

  # assign cumulative percent passing to local variable based on its value in global environment

  cumulative_percent_passing <- get("cumulative_percent_passing", envir = rlang::caller_env() )

  usga_bins_plus_gravel <- cumulative_percent_passing %>%
    dplyr::filter(microns >= 53) %>%
    tidyr::pivot_wider(names_from = microns, values_from = percent_passing) %>%
    psa_decimal_to_pct() %>%
    dplyr::mutate(
      gravel = `6730` - `2000`,
      very_coarse_sand = `2000` - `1000`,
      coarse_sand = `1000` - `500`,
      medium_sand = `500` - `250`,
      fine_sand = `250` - `150`,
      very_fine_sand = `150` - `53`) %>%
    psa_remove_number_bins()


  return(usga_bins_plus_gravel)

}

#' (Internal)
#'
#' Size bins for USCS breakdown
#'
USCS_bins <- function(){

  # assign cumulative percent passing to local variable based on its value in global environment

  cumulative_percent_passing <- get("cumulative_percent_passing", envir = rlang::caller_env() )


  # applies the 100x transformation across any columns whose
  # names begin with a number, then does the subtraction and finally removes
  # the columns whose names begin with a number

  uscs_bins <- cumulative_percent_passing %>%
    dplyr::filter(microns >= 53) %>%
    tidyr::pivot_wider(names_from = microns, values_from = percent_passing) %>%
    psa_decimal_to_pct() %>%
    dplyr::mutate(
      uscs_gravel = `4750` - `4000`,
      uscs_coarse_sand = `4000` - `2000`,
      uscs_med_sand = `2000` - `425`,
      uscs_fine_sand = `425` - `53`) %>%
    psa_remove_number_bins()



  return(uscs_bins)

}


#' (Internal)
#'
#' Size bins for USGA sieves
#'
#'
#' @return a tibble
#'
expanded_sieve_bins_1 <- function(){

  #  browser()

  # assign cumulative percent passing to local variable based on its value in global environment

  cumulative_percent_passing <- get("cumulative_percent_passing", envir = rlang::caller_env() )

  expanded_sieve_bins <- cumulative_percent_passing %>%
    dplyr::filter(microns >= 53) %>%
    tidyr::pivot_wider(names_from = microns, values_from = percent_passing) %>%
    psa_decimal_to_pct() %>%
    dplyr::mutate(
      gravel = `6350` - `2000`,
      very_coarse_sand = `2000` - `1000`,
      coarse_sand = `1000` - `500`,
      medium_sand = `500` - `250`,
      fine_sand = `250` - `150`,
      very_fine_sand = `150` - `53`) %>%
    psa_remove_number_bins()


  return(expanded_sieve_bins)

}


#' (Internal)
#' Size bins for pipette sampling with 20, 5, 2, and 0.2 microns
#'
#'
#'@return a tibble
#'
CSSC_fines_bins <- function(){

  # assign cumulative percent passing to local variable based on its value in global environment

  cumulative_percent_passing <- get("cumulative_percent_passing", envir = rlang::caller_env() )

# this pivots wider, computes the differences, then removes any columns whose names
# begins with a number

  CSSC_fines_bins <- cumulative_percent_passing %>%
    dplyr::filter(microns <= 53) %>%
    tidyr::pivot_wider(names_from = microns, values_from = percent_passing) %>%
    psa_decimal_to_pct() %>%
    dplyr::mutate(
     coarse_silt = `53` - `20`,
    medium_silt = `20` - `5`,
    fine_silt = `5` - `2`,
    # fine_silt = `20` - `2`, # this is DuraEdge's split; it's not official
     coarse_clay = `2` - `0.2`,
     fine_clay = `0.2`) %>%
    psa_remove_number_bins()

  return(CSSC_fines_bins)

}


#' (Internal)
#' Size bins for pipette sampling with 20, 2, and 0.2 microns
#'
#'
#' @details These are the silt bins used by DuraEdge/Natural Sand Co.,
#' plus the conventional coarse vs fine clay bins computed by pipette
#' sampling
#'
#'@return a tibble
#'
non_standard_fines_bins_1 <- function(){

  # assign cumulative percent passing to local variable based on its value in global environment

  cumulative_percent_passing <- get("cumulative_percent_passing", envir = rlang::caller_env() )

  # this pivots wider, computes the differences, then removes any columns whose names
  # begins with a number

  fines_bins <- cumulative_percent_passing %>%
    dplyr::filter(microns <= 53) %>%
    tidyr::pivot_wider(names_from = microns, values_from = percent_passing) %>%
    psa_decimal_to_pct() %>%
    dplyr::mutate(
      coarse_silt = `53` - `20`,
      fine_silt = `20` - `2`,
      coarse_clay = `2` - `0.2`,
      fine_clay = `0.2`) %>%
    psa_remove_number_bins()

  return(fines_bins)

}

#' (Internal)
#' Size bins for pipette sampling with 20, 2, and 0.2 microns
#'
#'
#' @details These are the silt bins used by DuraEdge/Natural Sand Co.,
#' plus clay (no further splitting of clay fraction)
#'
#'@return a tibble
#'
non_standard_fines_bins_2 <- function(){

  # assign cumulative percent passing to local variable based on its value in global environment

  cumulative_percent_passing <- get("cumulative_percent_passing", envir = rlang::caller_env() )

  # this pivots wider, computes the differences, then removes any columns whose names
  # begins with a number

  fines_bins <- cumulative_percent_passing %>%
    dplyr::filter(microns <= 53) %>%
    tidyr::pivot_wider(names_from = microns, values_from = percent_passing) %>%
    psa_decimal_to_pct() %>%
    dplyr::mutate(
      coarse_silt = `53` - `20`,
      fine_silt = `20` - `2`,
      clay = `2`) %>%
    psa_remove_number_bins()

  return(fines_bins)

}


#' (Internal)
#'
#' Size bins for pipette sampling for 2, and 0.2 microns
#'
#'
#'@return a tibble
#'
fines_20_to_0.2_only <- function(){

  # assign cumulative percent passing to local variable based on its value in global environment

  cumulative_percent_passing <- get("cumulative_percent_passing", envir = rlang::caller_env() )



  clay_bins <- cumulative_percent_passing %>%
    dplyr::filter(microns <= 53) %>%
    tidyr::pivot_wider(names_from = microns, values_from = percent_passing) %>%
    psa_decimal_to_pct() %>%
    dplyr::mutate(
      coarse_silt = `53` - `20`,
      fine_silt = `20` - `2`,
      coarse_clay = `2` - `0.2`,
      fine_clay = `0.2`) %>%
    psa_remove_number_bins()

  return(clay_bins)

}


#' Dummy data frame with no cases for 270 wash-through protocols
#'
#' (Internal)
#' @return tibble with no cases but having same names as other
#' datasheets to allow joining and binding
#'
wash_through_fines_df <- function(){

  # browser()

  needed_objs <- mget(x = c("common_datafiles"),
                      envir = rlang::caller_env())

  # make them available in the current function call
  list2env(needed_objs,envir = rlang::current_env())

dummy_fines_percent_passing <- common_datafiles$metadata %>%
  dplyr::mutate(
    microns = NA,
    percent_passing = NA
  )

return(dummy_fines_percent_passing)

}

#' Only gravel (<4000 microns), sand, silt, and clay reported
#'
#' Use for every sample; when no complex bins are reported this
#' is still run for the simple bins list element
#'
#' @return a tibble
#'
simple_bins <- function(){

  # assign the needed object from parent frame instead of
  # passing them as arguments

 # browser()

  cumulative_percent_passing <- get("cumulative_percent_passing", envir = rlang::caller_env() )
  protocol_ID <- get("protocol_ID", envir = rlang::caller_env() )

simple_size_bins <- cumulative_percent_passing %>%
    tidyr::pivot_wider(names_from = microns,
                       values_from = percent_passing) %>%
    dplyr::mutate(
      protocol_ID = .env$protocol_ID,
      gravel = `4000` - `2000`,
      sand = `2000` - `53`,
      silt = `53` - `2`,
      clay = `2`) %>%
    dplyr::select(
      date,
      experiment_name,
      sample_name,
      replication,
      batch_sample_number,
      protocol_ID,
      gravel,
      sand,
      silt,
      clay) %>%
    dplyr::mutate(
      dplyr::across(
        .cols = c(gravel, sand, silt, clay),
        .fns = ~.*100))

  return(simple_size_bins)

}
