#'\lifecycle{experimental} Simplify the process of adding water to soil
#'compaction specimens
#'
#'When preparing soil for compaction testing, it is often desired to mix the
#'specimens to known water contents, rather than "eyeballing" the water
#'additions. This ensures the water contents of the test specimens tested will
#'bracket the optimum water content and be spaced along a roughly equal range.
#'Some _a priori_ knowledge about the soil is needed to predict what this range
#'will be. The function allows the user to specify the range of water contents
#'that should be obtained for the test along with the anticipated maximum
#'density, and it returns information about the mass of soil to prepare and how
#'much water to add to each specimen.
#'
#'Using a battery-powered spray bottle increases the uniformity of water
#'throughout the soil, and improves the efficiency of the process. The
#'`spray_flo_rate_cm3` argument allows the user to specify the flow rate of the
#'bottle, if used.
#'
#'@details Many tests are often performed in the same day. In this case it is
#'  recommended to construct a tibble containing columns named `soil_ID`,
#'  `w_extant`, and `est_w_opt` and then use `purrr::pmap()` with this tibble as
#'  the `.l` argument. This action iteratively calls `compaction_aliquots()` for
#'  each row in the tibble and generates a tibble of the relevant info for each
#'  sample. This list can then be reduced in to a single tibble with `reduce(.f=
#'  rbind)` for saving to disk or printing. If multiple compaction efforts are
#'  also needed, repeat the above operation with different inputs for the
#'  alternative compaction efforts.
#'
#'@param soil_ID unique identifier for the soil being tested
#'@param w_extant current water content (g/g)
#'@param est_w_opt estimated optimum water content for the effort to be tested
#'  (g/g); usually this is ~90% of the plastic limit.
#'@param w_int water content interval between successive compaction points,
#'  defaults to 0.0125 (i.e. 1.25%)
#'@param assumed_d_max a conservatively high estimate of the maximum density
#'  that will be achieved in this test
#'@param n_cylinders number of compaction points to prepare, defaults to 6
#'@param cylinder_volume_cm3 volume of the compaction mold in cm^3^
#'@param spray_flo_rate_cm3 flow rate of bottle sprayer in cm^3^
#'
#'@details The `assumed_d_max` argument is used to ensure enough soil is mixed
#'  for each water content. All the aliquots use the same oven-dry soil mass for
#'  mixing purposes; more soil will be left for points very dry or wet of
#'  optimum
#'
#'@return a tibble with one row per aliquot
#'@export
#'
#'@example inst/examples/compaction_aliquots_example.R
#'
#'@seealso [`proctor_fit()`], [`mix_calcs()`]
#'
#'@export
#'
#'@references \href{https://www.astm.org/Standards/D698.htm}{ASTM D698-12e2}

compaction_aliquots <- function(soil_ID = NULL, w_extant = NULL,
                                        est_w_opt = NULL,w_int = 0.0125,
                                        assumed_d_max = 2.24,n_cylinders=5,
                                        cylinder_volume_cm3 = 937.4,
                                        spray_flo_rate_cm3 = 2.40){


  # error messages if required arguments are not present
  if(missing(soil_ID)){
    stop('\n\nNo soil_ID specified.')
  }

  if(missing(w_extant)){
    stop('\n\nNo w_extant value specified.')
  }

  if(missing(est_w_opt)){
    stop('\n\nNo est_w_opt specified.')
  }

  aliquot_masses <- tibble::tibble(
    soil_ID = soil_ID,
    cylinder_number = 1:n_cylinders,
    w_desired = c(
      (est_w_opt - 2*w_int),
      (est_w_opt - w_int),
      est_w_opt,
      (est_w_opt + w_int),
      (est_w_opt + 2*w_int) ),
    OD_soil_to_use = cylinder_volume_cm3*assumed_d_max,
    moist_soil_to_use = .data$OD_soil_to_use*(1+w_extant),
    w_desired_g = .data$w_desired*.data$OD_soil_to_use,
    w_already_present = .data$moist_soil_to_use - .data$OD_soil_to_use,
    w_to_add_g = .data$w_desired_g - .data$w_already_present,
    sec_to_spray= round(.data$w_to_add_g/spray_flo_rate_cm3, 0),
    sec_to_spray_period= lubridate::as.period(lubridate::as.duration(.data$sec_to_spray)),
    time_to_spray= sprintf('%02d:%02d',
                           lubridate::minute(.data$sec_to_spray_period),
                           lubridate::second(.data$sec_to_spray_period)  )
  ) %>%
    dplyr::select(
      .data$soil_ID,
      .data$cylinder_number,
      .data$w_desired,
      .data$moist_soil_to_use,
      .data$w_to_add_g,
      .data$time_to_spray
    ) %>%
    dplyr::rename(
      `Moist soil (g)` =.data$moist_soil_to_use,
      `H2O to add (g)` = .data$w_to_add_g,
      `Spray time (mm:ss)` = .data$time_to_spray
    ) %>%
    tibble::add_row(soil_ID= NA)

  return(aliquot_masses)

}


