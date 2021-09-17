#' Produce a soil with a known silt-to-clay ratio
#'
#' Compute air-dry component masses to use based on silt-size and clay-size mass
#' percentages and extant water contents

#'\loadmathjax In general, the return value of this function will be further
#'processed and passed to `mix_calcs()`. This would be useful when blending with
#'sand to yield a 3-component mixture with known sand content and SCR
#'
#' @param final_OD_kg oven-dry mass of particles < 53 microns in final mixture (in kg)
#' @param scr silt-to clay ratio, computed as \mjeqn{\frac{percent silt-size}{percent clay-size}}{}
#' @param silt_silty percent silt in the silty soil (decimal)
#' @param clay_silty percent clay in the silty soil (decimal)
#' @param silt_clayey percent silt in the clayey soil (decimal)
#' @param clay_clayey percent clay in the clayey soil (decimal)
#' @param w_silty gravimetric water content of silty soil (decimal)
#' @param w_clayey gravimetric water content of clayey soil (decimal)
#'
#' @return a tibble with two columns; the air-dry mass of component A (the silty soil) and component B (the clayey soil)
#' @export
#'
#' @example ./R/examples/scr_mix_calcs.R
#'
scr_mix_calcs <- function(final_OD_kg, scr, silt_silty, clay_silty, silt_clayey, clay_clayey, w_silty, w_clayey, include_sand_contents = FALSE, include_OD_masses = FALSE){

  # browser()



  # This function calculates the masses of air-dry components
  # needed to generate a known oven-dry mass of _fines_, not
  # of total soil A + soil B.....in other words, it accounts for
  # the "extra" mass of sand that is simply "along for the ride",
  # just as potassium might be when applying N-P-K fertilizer at
  # an N-based rate.

  # the user has the option to return only the air-dry component
  # masses or to also include the oven-dry masses and the sand content

  # the convention will be to consider soil A
  # the siltier material.

  # first need to calculate mass % of soil A and soil B

  # recode scr as R for easier typing

  R <- scr


  Mb <- ( (R * clay_silty) - silt_silty) / (silt_clayey - silt_silty + (R * clay_silty) - (R * clay_clayey) )

  Ma <- 1 - Mb

  # calculate correction factor to use some "extra" of each
  # soil so that the total mass of soil contains the needed
  # mass of OD fines

  final_OD_sand_pct <- (Mb * (1 - silt_clayey - clay_clayey) ) + (Ma * (1 - silt_silty - clay_silty) )

  final_OD_clay_pct <- (1- final_OD_sand_pct) / (1 + R)

  final_OD_silt_pct <- 1 - final_OD_sand_pct - final_OD_clay_pct

  sand_multiplier <- 1 + ( final_OD_sand_pct / (1 - final_OD_sand_pct))


  # the masses above are all on an oven-dry basis, so correct for
  # hygroscopic water contents

  Mb_air_dry_uncorrected <- Mb * (1 + w_clayey)
  Ma_air_dry_uncorrected <- Ma * (1 + w_silty)

  Mb_air_dry <- Mb_air_dry_uncorrected * sand_multiplier * final_OD_kg
  Ma_air_dry <- Ma_air_dry_uncorrected * sand_multiplier * final_OD_kg

# compute water content of final mix
  total_contained_water <- sum(Mb_air_dry, Ma_air_dry) - (sand_multiplier * final_OD_kg)
  final_mix_w <- total_contained_water/ (sand_multiplier * final_OD_kg)


  # compile everything to return

  return_list <- list(
    air_dry_component_masses = tibble::tibble(
      air_dry_kg_silty = Ma_air_dry,
      air_dry_kg_clayey = Mb_air_dry),
    OD_component_ratios = tibble::tibble(
      OD_kg_silty = Ma,
      OD_kg_clayey = Mb),
    final_OD_sand_pct = final_OD_sand_pct,
    final_mix_w = final_mix_w
  )

  return(return_list)

}

