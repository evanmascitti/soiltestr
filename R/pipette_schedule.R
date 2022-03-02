#' Compute the sampling times for a pipette analysis
#'
#' A generalized computation for any Stoke's equivalent spherical diameter
#'
#' @param sample_name Character, identifies sample name
#' @param start_time End of stirring period for cylinder, defaults to the day following function call at 8 AM; must be a datetime object
#' @param sample_depth_cm numeric, pre-determined sample depth, defaults to 10 cm
#' @param microns numeric vector of equivalent spherical diameters to sample
#' @param Gs Numeric, specific gravity of soil particles, defaults to 2.7
#' @param ambient_temp_c test water temperature
#'
#' @return tibble of ESD values (in microns) and absolute physical times to sample (in system time zone)
#' @export
#'
#' @importFrom rlang `%||%`
#'

pipette_schedule <- function(sample_name, start_time = NULL, sample_depth_cm = 10, microns = c(20, 5, 2), Gs, ambient_temp_c){

  test_date <- Sys.Date() + 1
  start_time <- start_time %||% lubridate::make_datetime(year = lubridate::year(test_date), month = lubridate::month(test_date), day = lubridate::day(test_date), hour = 8, min = 0, sec = 0, tz = Sys.timezone())


  # start_time,
  # n_reps,
  ###

  water_density <- unlist(h2o_properties_w_temp_c[dplyr::near(h2o_properties_w_temp_c$water_temp_c, ambient_temp_c), 'water_density_kg_m3'])



  water_viscosity <- unlist(h2o_properties_w_temp_c[dplyr::near(h2o_properties_w_temp_c$water_temp_c, ambient_temp_c), 'water_absolute_viscosity_poises']) / 10

  gravity <- 9.80665

  numerator <- unname((18 *  water_viscosity * sample_depth_cm))

  denominator <- gravity * ((microns * (10^-6))^2) * ((Gs * 1000)-water_density)

  duration_sec <- (numerator / denominator) / 100 # what's this 100 for ??

  duration_min <- duration_sec / 60

  duration_hr <- duration_min / 60
  # sample_depth <- ((duration_hr*60*60*gravity*((particle_diameter*10^-6)^2)*((Gs*1000)-(water_density))) / (18*water_viscosity) )*100

  ###

  # browser()

  # tibble::tibble(
  #   particle_diameter = microns,
  #   duration_hr = duration_hr,
  #   Gs = .env$Gs,
  #   ambient_temp_c = .env$ambient_temp_c
  # ) %>%
  #   purrr::pmap(pipette_sample_depth)
  # settling_times_hr <- mapply(
  #   FUN = pipette_sample_depth,
  #   particle_diameter = microns,
  #   duration_hr = duration_hr,
  #   MoreArgs = list(Gs = Gs, ambient_temp_c = ambient_temp_c)
  # )
  #
  # settling_times_sec <- settling_times_hr  * (60^2)

  return_tbl <- tibble::tibble(
    sample_name = sample_name,
    ESD = microns,
    sample_time = format(start_time + duration_sec, format = "%X", tz = Sys.timezone())
  )


  return(return_tbl)

  }
