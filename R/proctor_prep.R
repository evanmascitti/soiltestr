#'\lifecycle{experimental}
#'@title Simplify the process of adding water to soil compaction specimens
#'
#'@description Pass in a tibble of specimen information and compute the amount
#'  of water to add via bulk addition or spray bottle
#'
#'@details #'When preparing soil for compaction testing, it is often desired to
#'  mix the specimens to known water contents, rather than "eyeballing" the
#'  water additions. This ensures the water contents of the test specimens
#'  tested will bracket the optimum water content and be spaced along a roughly
#'  equal range. Some _a priori_ knowledge about the soil is needed to predict
#'  what this range will be. `compaction_aliquots()` allows the user to specify
#'  the interval between 5 successive water contents to use along with the
#'  anticipated maximum density. Returned table contains information about the
#'  mass of soil to prepare and how much water to add.
#'
#'  Multiple tests are often performed in the same day and at multiple
#'  compaction efforts. For this reason, the data frame passed to
#'  `compaction_aliquots()` should contain the following columns: - `effort`: a
#'  character string specifying the type of compactive effort, i.e. "standard",
#'  "modified", or "reduced" - `sample_ID` a unique identifier between the
#'  samples tested, i.e. mix number or name - `w_extant` the current water
#'  content of the soil (g/g) - `est_w_opt` the estimated optimum water content
#'  for the compaction effort to be tested.
#'  w\ifelse{html}{\out{<sub>opt</sup>}}{\eqn{_{opt}}{}} for the standard effort
#'  is tyipcally ~ 90% of the plastic limit and
#'  w\ifelse{html}{\out{<sub>opt-modified</sup>}}{\eqn{_{opt-modified}}{}} is
#'  3-4 % below
#'  w\ifelse{html}{\out{<sub>opt-standard</sup>}}{\eqn{_{opt-standard}}{}}.
#'
#'  This data frame can be easily prepared using [`dplyr::left_join()`] if the
#'  data are already in R, as demonstrated in **Examples**, or by using
#'  [`tidyr::crossing()`] to generate all combinations of `effort` and
#'  `sample_ID`, then adding a data frame column containing the water contents.
#'  The standard `w_opt`can be estimated in one of two ways: - Gradually add
#'  water to a ~ 25 mL soil specimen until it appears close to the optimum water
#'  content. Take a representative sample and measure the water content via
#'  oven-drying [ASTM 2216 - 19](https://www.astm.org/Standards/D2216.htm) -
#'  Perform a plastic limit test via [ASTM D
#'  4318](https://www.astm.org/Standards/D4318) and estimate
#'  w\ifelse{html}{\out{<sub>opt-standard</sup>}}{\eqn{_{opt-standard}}{}} as
#'  90% of this value. This routine is demonstrated in **Examples***
#'
#'  Using a battery-powered spray bottle increases the uniformity of water
#'  throughout the soil, and improves the efficiency of the process. The
#'  `spray_flo_rate_cm3_sec` argument allows the user to specify the flow rate
#'  of the bottle, if used.
#'
#'  The `assumed_d_max` argument is used to ensure enough soil is mixed for each
#'  water content. All the aliquots use the same oven-dry soil mass for mixing
#'  purposes; more soil will be left for points very dry or wet of optimum.
#'
#'@param df a data frame, see Details for required columns
#'@param w_int water content interval between successive compaction points,
#'  defaults to 0.0125 (i.e. 1.25%)
#'@param assumed_d_max a conservatively high estimate of the maximum density
#'  that will be achieved in this test
#'@param cylinder_volume_cm3 volume of the compaction mold in
#'  cm\ifelse{html}{\out{<sup>3</sup>}}{\eqn{^3}{^3}}
#'@param spray_flo_rate_cm3_sec flow rate of bottle sprayer in
#'  cm\ifelse{html}{\out{<sup>3</sup>}}{\eqn{^3}{^3}}/second
#'
#'
#'@return a tibble with one row per aliquot
#'@export
#'
#'@example inst/examples/proctor_prep_example.R
#'
#'@seealso [`proctor_fit()`], [`mix_calcs()`]
#'
#'@export
#'
#'@references \href{https://www.astm.org/Standards/D698.htm}{ASTM D698-12e2}

proctor_prep <- function(df, w_int = 0.0125, assumed_d_max = 2.24,
                           cylinder_volume_cm3 = 937.4,
                           spray_flo_rate_cm3_sec = 2.40){

  # error messages if required arguments are not present
  if(missing(df)){
    stop('\n\nNo data frame provided in `df` argument.')
  }

  if(! "effort" %in% names(df)){
    stop('\n\nNo `effort` column present in `df`.')
  }

  if(! "sample_ID" %in% names(df)){
    stop('\n\nNo `sample_ID` column present in `df`.')
  }

  if(! "w_extant" %in% names(df)){
    stop('\n\nNo `w_extant` value present in `df`.')
  }

  if(! "est_w_opt" %in% names(df)){
    stop('\n\nNo `est_w_opt` present in `df`.')
  }

  # generate new data frame

  newdf <- df %>%
    dplyr::group_by(.data$sample_ID, .data$effort) %>%
    dplyr::mutate(aliquots = purrr::pmap(.l= list(w_extant = .data$w_extant, est_w_opt = .data$est_w_opt),
                                  .f= ~tibble(
                                    cylinder_number = 1:5,
                                    w_desired= c(est_w_opt - 0.025,
                                                 est_w_opt - 0.0125,
                                                 est_w_opt,
                                                 est_w_opt + 0.0125,
                                                 est_w_opt + 0.025 ),
                                    OD_soil_to_use = cylinder_volume_cm3*assumed_d_max,
                                    delta_w = .data$w_desired - w_extant,
                                  moist_soil_to_use = .data$OD_soil_to_use*(1+w_extant),
                                  w_desired_g = .data$w_desired*.data$OD_soil_to_use,
                                  w_already_present = .data$moist_soil_to_use - .data$OD_soil_to_use,
                                  w_to_add_g = .data$w_desired_g - .data$w_already_present,
                                  sec_to_spray= round(.data$w_to_add_g/spray_flo_rate_cm3_sec, 0),
                                  sec_to_spray_period= lubridate::as.period(lubridate::as.duration(.data$sec_to_spray)),
                                  time_to_spray= sprintf('%02d:%02d',
                                                         lubridate::minute(.data$sec_to_spray_period),
                                                         lubridate::second(.data$sec_to_spray_period)
    ) ) ) ) %>%
    tidyr::unnest(.data$aliquots) %>%
    dplyr::ungroup() %>%
    dplyr::select(
      .data$effort, .data$sample_ID, .data$moist_soil_to_use,
      .data$w_to_add_g, .data$cylinder_number, .data$time_to_spray )

  return(newdf)
}

