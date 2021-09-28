#'`r lifecycle::badge('experimental')`
#'
#'@title Simplify the process of adding water to soil compaction specimens
#'
#'@description Pass in a tibble of specimen information and compute the amount
#'  of water to add for each aliquot.
#'
#'@details When preparing soil for compaction testing, it is often desired to
#'  mix the specimens to known water contents, rather than "eyeballing" the
#'  water additions. This ensures the water contents of the test specimens
#'  tested will bracket the optimum water content and be spaced along a roughly
#'  equal range. Some _a priori_ knowledge about the soil is needed to predict
#'  what this range will be. `proctor_prep()` allows the user to specify
#'  the interval between 5 successive water contents to use along with the
#'  anticipated maximum density and the plastic limit of the soil. The maximum
#'  density for the standard effort test is estimated as 95% of w~~opt~~.
#'  w~~opt~~ for the modified test is estimated as 0.75 x w~~opt standard~~.
#'  Returned table contains information about the mass of soil to prepare and
#'  how much water to add.
#'
#'
#'@param x S3 object of class `sand_scr_mix_tbl`
#' @param pl Plastic limit values. If `x` is provided, `pl` should be a data frame having two columns: the same sample names and a column titled `pl`. If `x` is `NULL`, `pl` should be a numeric vector of the same length as`sample_name`.
#'@param date Date of mixing or testing.
#'@param sample_name Character vector of unique sample identifiers.
#'@param w_extant Existing water content values. If `x` is provided, can be ignored. If `x` is `NULL` (the default), should be a numeric vector of same length as `sample_name`.
#'@param effort Character vector of compaction efforts. May be one or more of 'standard', 'modified', or 'reduced'.
#'@param w_int Gravimetric water content interval between successive compaction points. Defaults to 0.015 (1.5%)
#'@param assumed_d_max A conservatively high estimate of the maximum density
#'  that will be achieved in this test. Defaults to 2.25
#'  g/cm\ifelse{html}{\out{<sup>3</sup>}}{\eqn{^3}{^3}} to ensure enough soil is
#'  wetted for all aliquots.
#' @param n_cylinders Number of specimens to prepare per test. Defaults to 6. 5 specimens is considered the minimum, but more or tighter-spaced cylinders can better define the compaction curve.
#'@param cylinder_volume_cm3 volume of the compaction mold in
#'  cm\ifelse{html}{\out{<sup>3</sup>}}{\eqn{^3}{^3}}. Used to compute compacted
#'  volume of soil plus 0.25" of "over-pack" extending above the top of the
#'  4.5"-high mold.
#'
#'@return A tibble with one row per aliquot (tidy format). If there are soils with extant water contents above the lowest required for the test, these cases will populate with negative values for `water_to_add_g`
#'@export
#'
#'@example inst/examples/proctor_prep_example.R
#'
#'@seealso [`proctor_fit()`], [`mix_calcs()`], [`proctor_datasheet()`]
#'
#'@export
#'
#'@references \href{https://www.astm.org/Standards/D698.htm}{ASTMc D698-12e2}

proctor_prep <- function(x = NULL,
                         pl = NULL,
                         date = Sys.Date(),
                         sample_name,
                         w_extant = NULL,
                         effort = c('standard', 'modified'),
                         w_int = 0.015,
                         assumed_d_max = 2.25,
                         n_cylinders = 6,
                         cylinder_volume_cm3 = 940){


 #  browser()


  # Some error messages to check for arguments if an object is not
  # passed to x

  if(is.null(x)){

    if (class(date) != 'character' && class(date) != 'Date') {
    stop('\n\n `date` argument not understood, did you forget quotation marks?')
  }

  if(missing(w_extant)){
      stop("`w_extant` argument not provided. Should be a data frame with columns `sample_name` and `w_extant`, or (if `x` is NULL) a numeric vector of equal length to `sample_name`")
    }

  }



  # code for when x is provided begins here  ------------------------------

if(!is.null(x)){

  if(missing(pl)){
    stop("No data frame provided for `PL` argument.")
  }

  base_df <- x

  efforts_df <- tidyr::crossing(
    sample_name = unname(x$sample_name),
    effort = effort
  )


 #  browser()

# having some problems with method inheritance and dplyr::across(), but by converting
# to a data frame it seems to strip the other classes off (via as.data.frame()), which is fine because the class is no longer needed here....then
# able to use dplyr verbs again

  w_targets_df <- purrr::reduce(
    .x = list(base_df, efforts_df, pl),
    .f = dplyr::left_join,
    by = 'sample_name'
  ) %>%
    as.data.frame() %>%
    dplyr::select(.data$sample_name, .data$effort, .data$w_extant, .data$PL) %>%
    dplyr::group_by(dplyr::across()) %>%
    dplyr::mutate(
      w_target = purrr::map(PL, make_w_spread)
    ) %>%
    dplyr::ungroup() %>%
    tidyr::unnest(cols = c(.data$w_target)) %>%
    dplyr::ungroup()

  adjusted_w_targets_df <- adjust_w_targets(x = w_targets_df)

 #  browser()

  augmented_w_targets_df <- augment_proctor_w_targets(x = adjusted_w_targets_df)

  return(augmented_w_targets_df)

}

 # code for when x is provided ends here ---------------------------------



# code for when x is NOT provided begins here -----------------------------

 # browser()


  if(missing(pl)){
    stop("No values provided for `pl`. Please provide either a numeric vector of equal length to `sample_name`, or (instead) supply a data frame in `x`.")
  }


  pl_df <- tibble::tibble(
    sample_name = unname(sample_name),
    w_extant = unname(w_extant),
    PL  = unname(pl)
  )



  # generate the new water contents with the helper function `make_w_spread()`,
  w_targets_df <- tidyr::crossing(
     sample_name = unname(sample_name),
     effort = effort) %>%
     dplyr::left_join(pl_df, by = 'sample_name') %>%
     dplyr::group_by(dplyr::across()) %>%
    dplyr::mutate(w_target = purrr::map(PL, make_w_spread)) %>%
    tidyr::unnest(cols = .data$w_target) %>%
    dplyr::ungroup()


   # then adjust the reduced or modified water content as needed

  adjusted_w_targets_df <- adjust_w_targets(x = w_targets_df)


  augmented_w_targets_df <- augment_proctor_w_targets(x = adjusted_w_targets_df)

   return(augmented_w_targets_df)

}



#' Helper function for generating proctor water contents
#'
#' @param n_cyls Number of specimens to prepare
#'
#' @return Tibble containing existing water content, anticipated optimum water content for standard test, and target water content for each specimen
#'
make_w_spread <- function(PL, n_cyls = NULL){

  # browser()

  # find n_cylinders and w_int arguments from parent call

  n_cyls <- get("n_cylinders", envir = rlang::caller_env(n = 2))

  w_int <- get("w_int", envir = rlang::caller_env(n = 2))

  if(!n_cyls %in% c(5:6)){
    stop("`n_cyls` argument is ", n_cyls, "but it must be either 5 or 6.",
         call. = FALSE)
  }


  if(n_cyls == 5L){
    new_water_contents <-  c(
      PL - w_int * 2,
      PL - w_int,
      PL,
      PL + w_int,
      PL + w_int * 2
    )} else {
      if(n_cyls == 6L){
        new_water_contents <- c(
          PL - (w_int * 3),
          PL - (w_int * 2),
          PL - w_int,
          PL,
          PL + w_int,
          PL + (w_int * 2)
          )

      }
    }

  # build tibble by recycling arguments that originally
  # came from the one-row tibble fed to this
  # function and tacking on the new target water contents vector

  # w_spread_tbl <- tibble::tibble(
  #   std_est_w_opt = std_est_w_opt,
  #   w_target = new_water_contents
  # )

  return(new_water_contents)
}


#' Add the target water contents to a nested data frame
#'
#' A helper for `make_w_spread`
#' @param w_targets_df
#'
#' @return An unnested tibble containing the original colums with the correct target water contents for each specimen
#'
adjust_w_targets <- function(x){

   w_targets_df <- x %>%
     dplyr::mutate(
      w_target = dplyr::case_when(
        effort == 'modified' ~ 0.75 * w_target, # typically the modified w_opt is about 70-75% of that for the standard effort
        effort == 'reduced' ~ 1.25 * w_target, # w_opt will be larger for reduced effort
        effort == 'standard' ~ w_target
      )
    )

  return(w_targets_df)

}


#' Does some additional calculations after making the new test water contents....i.e.. to actually yield the soil and water masses to use for each sample x effort combination
#'
#' It is a helper which is called after
#'
#' @return a tibble with relevant info
#'
augment_proctor_w_targets <- function(x){

  # browser()

  parent_objs <- mget(x = c("cylinder_volume_cm3", "assumed_d_max", "date"), envir = rlang::caller_env())

  list2env(parent_objs, envir = rlang::current_env())

  # split/apply/combine to make the cylinder numbers....
  # then compute the soil to use and water to add before
  # selecting only the relevant columns.

  augmented_w_targets_df <- x %>%
    split(~sample_name + effort) %>%
    purrr::map(~dplyr::mutate(., cylinder_number = 1:nrow(.))) %>%
    dplyr::bind_rows() %>%
    dplyr::mutate(
      date = date,
      OD_soil_to_use = cylinder_volume_cm3 * assumed_d_max * (4.75 / 4.5),
      delta_w = .data$w_target - w_extant,
      moist_soil_to_use_g = .data$OD_soil_to_use * (1 + w_extant),
      w_target_g = .data$w_target * .data$OD_soil_to_use,
      w_already_present = .data$moist_soil_to_use_g - .data$OD_soil_to_use,
      water_to_add_g = .data$w_target_g - .data$w_already_present
    ) %>%
    dplyr::select(
      .data$date,
      .data$sample_name,
      .data$effort,
      .data$w_target,
      .data$cylinder_number,
      .data$moist_soil_to_use_g,
      .data$water_to_add_g)  %>%
    dplyr::mutate(
        moist_soil_to_use_g = round(moist_soil_to_use_g, digits = 0),
        water_to_add_g = round(water_to_add_g, digits = 0)
      )


  return(augmented_w_targets_df)

}

