#'`r lifecycle::badge('experimental')`
#'
#'@title Simplify the process of adding water to soil compaction specimens
#'
#'@description Pass in a tibble of specimen information and compute the amount
#'  of water to add for each aliquot.
#'
#'@details The simplest way to use this function for splitting soil mixtures is to generate two tables.
# The first is generated using `sand_clay_mix_calcs()` or
# `sand_w_scr_mix_calcs()`. The second should contain the sample name and their
# plastic limits. These are passed to the `x` and `PLs` arguments. Other approaches are shown in the examples.
#'
#' The rationale for this function is efficient Proctor testing of soil mixtures.
#' When preparing samples for compaction testing, it is often desired to
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
#'@param x Data frame containing columns `sample_name`, `w_extant`, and `effort`. May also contain a column of plastic limit values, named `PL`. # S3 object of class `sand_scr_mix_tbl`
#' @param PLs Plastic limit values. If `x` contains a `PL` column, may be `NULL` (the default). If `x` is provided but does _not_ contain a `PL` column, `PLs` should be a data frame having two columns: the same sample names and a column titled `PLs`. If `x` is `NULL`, `PLs` should be a numeric vector of the same length as`sample_name`.
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
#'@return A tibble with one row per aliquot (tidy format). If there are soils with extant water contents above the lowest required for the test, these cases will populate with negative values for `water_to_add_g`. Soil masses are rounded to the nearest 100 g and water additions are rounded to the nearest 10 g.
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
                         PLs = NULL,
                         date = Sys.Date(),
                         sample_name,
                         w_extant = NULL,
                         effort = c('standard', 'modified'),
                         w_int = 0.015,
                         assumed_d_max = 2.25,
                         n_cylinders = 6,
                         cylinder_volume_cm3 = 940){


   # browser()


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

  if(missing(PLs) & ! 'PL' %in% names(x)){
    stop("No data frame provided for `PL` argument.")
  }

  if('effort' %in% names(x)){

    base_df <- x

    warning("`effort` column present in `x`; ignoring `effort` argument", call. = FALSE)

  } else{

    # need to add the efforts to the base data frame...
    # do this via joining

    effort <- match.arg(arg = effort, choices = c('reduced', 'standard', 'modified'), several.ok = TRUE)

    efforts_df <- tidyr::crossing(
      sample_name = unname(x$sample_name),
      effort = effort
    )

    base_df <- x %>%
      dplyr::left_join(efforts_df,
                       by = c('sample_name'))

    # now that base_df contains the proper
    # columns, function can continue

    }

# browser()

# having some problems with method inheritance and dplyr::across(), but by stripping the other class off it seems
  # to work again...need to learn more about method
  # inheritance ....now able to use dplyr verbs again

  # browser()


  # if the x argument already has a column for PL, no need to join
  # with the PL values data frame. Otherwise, join with data frame
  # provided in PLs argument

 #  browser()

  if('PL' %in% names(x)){
    w_targets_df <- base_df
    } else{
      w_targets_df <- dplyr::left_join(
        base_df,
        PLs,
        by = c('sample_name')
      )
    }

#  browser()

  w_targets_df <-  unclass(w_targets_df) %>%
    tibble::as_tibble() %>%
    dplyr::select(
      .data$sample_name,
      .data$effort,
      .data$w_extant,
      .data$PL) %>%
    dplyr::group_by(dplyr::across()) %>%
    dplyr::mutate(
      w_target = purrr::map2(effort, PL, make_w_spread)
    ) %>%
    dplyr::ungroup() %>%
    tidyr::unnest(cols = c(.data$w_target)) %>%
    dplyr::ungroup()

  augmented_w_targets_df <- augment_proctor_w_targets(x = w_targets_df)

  return(augmented_w_targets_df)

}

 # code for when x is provided ends here ---------------------------------



# code for when x is NOT provided begins here -----------------------------

#  browser()


  if(missing(PLs)){
    stop("No values provided for `PLs`. Please provide either a numeric vector of equal length to `sample_name`, or (instead) supply a data frame in `x`.")
  }


  # browser()

 pl_df <- tibble::tibble(
    sample_name = unname(sample_name),
    w_extant = unname(w_extant),
    PL  = unname(PLs)
  )

# browser()

  # generate the new water contents with the helper function `make_w_spread()`,
  w_targets_df <- tidyr::crossing(
     sample_name = unname(sample_name),
     effort = effort) %>%
     dplyr::left_join(pl_df, by = 'sample_name') %>%
    dplyr::group_by(dplyr::across()) %>%
    tidyr::nest() %>%
    dplyr::mutate(w_target = purrr::map2(effort, PL, make_w_spread)) %>%
    tidyr::unnest(cols = .data$w_target) %>%
    dplyr::ungroup()


  augmented_w_targets_df <- augment_proctor_w_targets(x = w_targets_df)

  return(augmented_w_targets_df)

}



#' Helper function for generating proctor water contents
#'
#' @param effort Character vector of length 1 indicating compaction effort
#' @param PL Estimated value for plastic limit or (optimum water content, for
#'   nonplastic soils)
#' @param n_cyls Number of specimens to prepare
#'
#' @return Tibble containing existing water content, anticipated optimum water content for standard test, and target water content for each specimen
#'
make_w_spread <- function(effort, PL, n_cyls = NULL){

 #  browser()

  # find n_cylinders and w_int arguments from parent call and re-assign
  # them in the current environment

  n_cyls <- get("n_cylinders", envir = rlang::caller_env(n = 2))

  w_interval <- get("w_int", envir = rlang::caller_env(n = 2))

  if(!n_cyls %in% c(5:6)){
    stop("`n_cyls` argument is ", n_cyls, "but it must be either 5 or 6.",
         call. = FALSE)
  }


  est_w_opt <- dplyr::case_when(
    effort == 'reduced' ~ 1.15 * PL,
    effort == 'standard' ~ PL,
    effort == 'modified' ~ 0.75 * PL
    )

  # This won't work because case_when() evaluates _all_ of the RHS expressions first,
  # then decides which to use based on the LHS condition. This seems backwards to me
  # but there must be a good reason for it. Anyway, you evidently can't put error messages
  # inside this function the same way you can for `switch()`. However in this case
  # I already have a check for effort using `match.arg()` earlier in the function call,
  # so no need for the extra check here.

    # ,
  #   TRUE ~ stop('Compaction effort must be one of "reduced", "standard", or "modified". The estimated optimum water content is derived from the effort and the plastic limit.')
  # )

#
#   if(effort == 'modified'){
#     est_w_opt <- 0.75 * PL
#   } else{
#     if(effort == 'standard'){
#       est_w_opt <- PL
#     } else{
#       if(effort == 'reduced'){
#         est_w_opt <- 1.25 * PL}
#       }
#   }


  if(n_cyls == 5L){
    new_water_contents <-  c(
      est_w_opt - w_interval * 2,
      est_w_opt - w_interval,
      est_w_opt,
      est_w_opt + w_interval,
      est_w_opt + w_interval * 2
    )} else {
      if(n_cyls == 6L){
        new_water_contents <- c(
          est_w_opt - (w_interval * 3),
          est_w_opt - (w_interval * 2),
          est_w_opt - w_interval,
          est_w_opt,
          est_w_opt + w_interval,
          est_w_opt + (w_interval * 2)
          )
      }
    }


  return(new_water_contents)
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
        moist_soil_to_use_g = round(moist_soil_to_use_g, digits = -1),
        water_to_add_g = round(water_to_add_g, digits = -1)
      )


  return(augmented_w_targets_df)

}

