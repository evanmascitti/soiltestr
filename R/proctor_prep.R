#'`r lifecycle::badge('experimental')`
#'
#'@title Simplify the process of adding water to soil compaction specimens
#'
#'@description Pass in a tibble of specimen information and compute the amount
#'  of water to add via bulk addition or spray bottle.
#'
#'@details When preparing soil for compaction testing, it is often desired to
#'  mix the specimens to known water contents, rather than "eyeballing" the
#'  water additions. This ensures the water contents of the test specimens
#'  tested will bracket the optimum water content and be spaced along a roughly
#'  equal range. Some _a priori_ knowledge about the soil is needed to predict
#'  what this range will be. `proctor_prep()` allows the user to specify
#'  the interval between 5 successive water contents to use along with the
#'  anticipated maximum density. Returned table contains information about the
#'  mass of soil to prepare and how much water to add.
#'
#'  Multiple tests are often performed in the same day and at multiple
#'  compaction efforts. For this reason, the data frame passed to
#'  `proctor_prep()` should contain the following columns:
#'
#'  - `sample_name` a unique identifier
#'  between the samples tested, i.e. mix number or name - `w_extant` the current
#'  water content of the soil (g/g)
#'  - `effort`: a character string specifying the type of compactive effort,
#'  i.e. "standard", "modified", or "reduced"
#'  - `est_w_opt` the estimated optimum water
#'  content for the compaction effort to be tested.
#'
#'
#'  Note that `w_opt` for the standard effort is commonly ~ 90% of the plastic
#'  limit. `w_opt` for the modified effort is typically 3-4 % below that for the
#'  standard effort.
#'
#'  The data frame passed to `x` can be easily prepared using
#'  [`dplyr::left_join()`] if the data are already in R, as demonstrated in
#'  the examples, or by using [`tidyr::crossing()`] to generate all combinations
#'  of `effort` and `sample_name`, then adding a data frame column containing the
#'  water contents. The standard `w_opt` can be estimated in one of two ways:
#'
#'  - Gradually add water to a ~ 25 mL soil specimen until it appears close to
#'  the optimum water content. Take a representative sample and measure the
#'  water content via oven-drying
#'  [ASTM 2216 - 19](https://www.astm.org/Standards/D2216.htm)
#'
#'  - Perform a plastic limit test and estimate `w_opt` as 90% of this value.
#'  This routine is demonstrated in the examples.
#'
#'  Using a battery-powered spray bottle increases the uniformity of water
#'  throughout the soil, and improves the efficiency of the process. The
#'  `spray_flo_rate_cm3_sec` argument allows the user to specify the flow rate
#'  of the bottle, if used.
#'
#'  The `assumed_d_max` argument is used to ensure enough soil is mixed for each
#'  water content. All the aliquots use the same oven-dry soil mass for mixing
#'  purposes; more soil will be left over for points very dry or wet of optimum.
#'
#'@param x a data frame, see Details for required columns
#'@param date date of actual compaction test
#'@param w_int water content interval between successive compaction points,
#'  defaults to 0.02 (i.e. 2%)
#'@param assumed_d_max a conservatively high estimate of the maximum density
#'  that will be achieved in this test. Defaults to 2.25.
#'  g/cm\ifelse{html}{\out{<sup>3</sup>}}{\eqn{^3}{^3}}
#'@param cylinder_volume_cm3 volume of the compaction mold in
#'  cm\ifelse{html}{\out{<sup>3</sup>}}{\eqn{^3}{^3}}. Used to compute compacted
#'  volume of soil plus 0.25" of "over-pack" extending above the top of the
#'  4.5"-high mold.
#'
#'@return A tibble with one row per aliquot. If there are soils with water
#'  content exceeding the minimum value desired, a negative number will populate
#'  the `w_to_add_g` column.
#'@export
#'
#'@example inst/examples/proctor_prep_example.R
#'
#'@seealso [`proctor_fit()`], [`mix_calcs()`], [`proctor_datasheet()`]
#'
#'@export
#'
#'@references \href{https://www.astm.org/Standards/D698.htm}{ASTMc D698-12e2}

proctor_prep <- function(date,
                         sample_name,
                         effort = c('standard', 'modified'),
                         w_extant,
                         est_w_opt,
                         w_int = 0.02,
                         assumed_d_max = 2.20,
                         n_cylinders = 5,
                         cylinder_volume_cm3 = 940){

  # error messages if required arguments are not present
  if(missing(x)){
    stop('\n\nNo data frame provided in `x` argument.')
  }

  if(missing(date)){
    stop('\n\nNo date provided')
  }

  if (class(date) != 'character' && class(date) != 'Date') {
    stop('\n\n `date` argument not understood, did you forget quotation marks?')
  }

  if(! "effort" %in% names(x)){
    stop('\n\nNo `effort` column present in `x`.')
  }

  if(! "sample_name" %in% names(x)){
    stop('\n\nNo `sample_name` column present in `x`.')
  }

  if(! "w_extant" %in% names(x)){
    stop('\n\nNo `w_extant` value present in `x`.')
  }

  if(! "est_w_opt" %in% names(x)){
    stop('\n\nNo `est_w_opt` present in `x`.')
  }


  std_est_w_opts <- tibble::tibble(
    sample_name = sample_name,
    w_extant,
    std_est_w_opt  = est_w_opt
  )


   base_df <-tidyr::crossing(
     sample_name = sample_name,
     effort = effort) %>%
     dplyr::left_join(std_est_w_opts, by = 'sample_name') %>%
     dplyr::group_by(sample_name, effort) %>%
     tidyr::nest()



   make_w_spread <- function(x, n_cyls = n_cylinders){

    if(n_cyls == 5L){
      new_water_contents <-  c(
        x$std_est_w_opt - w_int * 2,
        x$std_est_w_opt - w_int,
        x$std_est_w_opt,
        x$std_est_w_opt + w_int,
        x$std_est_w_opt + w_int * 2
      )} else {
      if(n_cyls == 6L){
        new_water_contents <- c(
          x$std_est_w_opt - w_int * 3,
          x$std_est_w_opt - w_int * 2,
          x$std_est_w_opt,
          x$std_est_w_opt + w_int,
          x$std_est_w_opt + w_int * 2,
          x$std_est_w_opt + w_int * 3)

      } else{
        stop("`n_cyls` argument is ", n_cyls, "but it must be either 5 or 6.",
             call. = FALSE)}
      }

     # build tibble by recycling arguments that originally
     # came from the one-row tibble fed to this
     # function and tacking on the new target water contents vector
     w_spread_tbl <- tibble::tibble(
       w_extant = x$w_extant,
       std_est_w_opt = x$std_est_w_opt,
       w_target = new_water_contents
     )

     return(w_spread_tbl)
   }


   # generate the new water contents with the helper function above,
   # then alter the reduced or modified water content as needed

   w_targets_tbl <- base_df %>%
     dplyr::mutate(
       w_targets_df = purrr::map(make_w_spread)
     ) %>%
     dplyr::ungroup() %>%
     tidyr::unnest(w_targets_df) %>%
     dplyr::mutate(
       w_target = dplyr::case_when(
         effort == 'modified' ~ 0.75 * w_target, # typically the modified w_opt is about 70-75% of that for the standard effort
         effort == 'reduced' ~ 1.25 * w_target, # w_opt will be larger for reduced effort
         effort == 'standard' ~ w_target
       )
     )

n_tests <- length(sample_name) * length(effort)


   new_df <- w_targets_tbl %>%
     dplyr::mutate(
       cylinder_number = rep(1:cylinder_number, times = n_tests),
       OD_soil_to_use = cylinder_volume_cm3 * assumed_d_max * (4.75 / 4.5),
       delta_w = .data$w_target - w_extant,
       moist_soil_to_use_g = .data$OD_soil_to_use * (1 + w_extant),
       w_target_g = .data$w_target * .data$OD_soil_to_use,
       w_already_present = .data$moist_soil_to_use_g - .data$OD_soil_to_use,
       w_to_add_g = .data$w_target_g - .data$w_already_present
       ) %>%
     dplyr::select(
       .data$sample_name,
       .data$effort,
       .data$date,
       .data$w_target,
       .data$cylinder_number,
       .data$moist_soil_to_use_g,
       .data$w_to_add_g) %>%
     dplyr::mutate(
       moist_soil_to_use_g = round(moist_soil_to_use_g, digits = -2),
       w_to_add_g = round(w_to_add_g, digits = -1)


# leaving off for today (2021-09-24)

gi

# create new water contents based on number of cylinders and water content interval

  # w_targets <- list(
  #   five_cyls = c(
  #     est_w_opt - w_int * 2,
  #     est_w_opt - w_int,
  #     est_w_opt,
  #     est_w_opt + w_int,
  #     est_w_opt + w_int * 2
  #   ),
  #   six_cyls = c(
  #     est_w_opt - w_int * 3,
  #     est_w_opt - w_int * 2,
  #     est_w_opt,
  #     est_w_opt + w_int,
  #     est_w_opt + w_int * 2,
  #     est_w_opt + w_int * 3)
  # )


  # generate new data frame
  # new_df <- x %>%
  #   dplyr::group_by(.data$sample_name, .data$effort) %>%
  #   dplyr::mutate(
  #     aliquots =
  #       purrr::pmap(
  #         .l = list(
  #           w_extant = .data$w_extant,
  #           est_w_opt = .data$est_w_opt
  #         ),
  #         .f = ~ tibble::tibble(
  #           date = lubridate::as_date(date),
  #           cylinder_number = 1:n_cylinders,
  #           w_target = dplyr::case_when(
  #             n_cylinders == 5 ~ w_targets$five_cyls,
  #             n_cylinders == 6 ~ w_targets$six_cyls
  #           ),
  #           OD_soil_to_use = cylinder_volume_cm3 *
  #             assumed_d_max * (4.75 / 4.5),
  #           delta_w = .data$w_target - w_extant,
  #           moist_soil_to_use_g = .data$OD_soil_to_use *
  #             (1 + w_extant),
  #           w_target_g = .data$w_target * .data$OD_soil_to_use,
  #           w_already_present = .data$moist_soil_to_use_g - .data$OD_soil_to_use,
  #           w_to_add_g = .data$w_target_g - .data$w_already_present
  #         )
  #       ) ) %>%
  #   tidyr::unnest(.data$aliquots) %>%
  #   dplyr::ungroup() %>%
  #   dplyr::select(
  #     .data$sample_name,
  #     .data$effort,
  #     .data$date,
  #     .data$w_target,
  #     .data$cylinder_number,
  #     .data$moist_soil_to_use_g,
  #     .data$w_to_add_g) %>%
  #   dplyr::mutate(
  #     moist_soil_to_use_g = round(moist_soil_to_use_g, digits = -2),
  #     w_to_add_g = round(w_to_add_g, digits = -1)
      )

  return(structure(new_df, class = "proctor_prep_sheet"))
}

