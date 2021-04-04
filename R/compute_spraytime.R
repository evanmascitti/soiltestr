#' @title Determine spray time for wetting a sample
#'
#' @description Once the optimum water content is determined, the bulk mixture must be brought to this water content. `compute_spraytime()` allows the user to pass a data frame of existing water contents (`w_target`) and target water contents; the return value is the existing data frame with water masses and spray times added. This makes the water addition process accurate and precise while keeping the workflow simple.
#'
#'
#' @param df Data fram containing columns for the unique identifier (typically `sample_name`) and the `w_extant` and `w_target` values (the latter two being gravimetric water contents as decimals)
#' @param OD_soil_mass_g the mass of soil to prepare, in terms of oven-dry mass. Defaults to 8000 g (enough to pack three chunk cylinders to a depth of 2", including 0.17" of over-packed soil, and a few hundred g remaining for daily repairs)
#' @param spray_flo_rate_cm3_sec flow rate of electric spray bottle, in cm3 per second.
#' @return Data frame containing the original columns, with columns added for the total water quantity and a spray time.
#' @export
#'
compute_spraytime <- function(df, OD_soil_mass_g = 8000,
                              spray_flo_rate_cm3_sec = 2.4) {

  df %>%
    dplyr::mutate(
      moist_mass_g = OD_soil_mass_g * (1+.data$w_target) ,
      water_already_present_g = OD_soil_mass_g * .data$w_extant ,
      water_desired_g = OD_soil_mass_g * .data$w_target ,
      w_to_add_g = .data$water_desired_g  - .data$water_already_present_g ,
      sec_to_spray = round(
        .data$w_to_add_g/spray_flo_rate_cm3_sec,0) ,
      sec_to_spray_period = lubridate::as.period(lubridate::as.duration(.data$sec_to_spray)),
      time_to_spray = sprintf("%02d:%02d",
                              lubridate::minute(.data$sec_to_spray_period),
                              lubridate::second(.data$sec_to_spray_period) )
    ) %>%
    dplyr::select(-c(.data$water_already_present_g, .data$water_desired_g, .data$sec_to_spray, .data$sec_to_spray_period))

  } # end of function

