#' `r lifecycle::badge('maturing')`
#'
#' @title Calculate soil mixture component weights
#'
#'\loadmathjax
#'@description Returns a concise, printer-friendly
#'reference sheet. It is useful when mixing two soils ("sand" and "clay") from
#'an air-dry condition to produce a final mixture having a particular %
#'sand-size particles. See **Details** for more info on the calculations.
#'
#' @param mix_date Date the mixture is being produced in yyyy-mm-dd
#' @param expt_mix_nums Character or numeric vector of the unique mix identifiers
#' @param sand_name Unique name assigned to the particular sand material (for
#'   example, "Boyd's Fine").
#' @param clay_name Unique name assigned to the particular sand material (for
#'   example, "LA black gumbo).
#' @param final_sand_pcts Numeric vector of equal length to
#'   \code{expt.mix.nums}. It corresponds to the final desired % of sand-size
#'   particles in the soil mixture on an oven-dry mass basis.
#' @param final_OD_kg  desired mass of mixed soil to obtain, on an oven-dry mass
#'   basis (in kilograms). Defaults to 43 kg. This is a good estimate of the
#'   mass needed to perform a standard Proctor test, a modified Proctor test,
#'   and to prepare 6 "chunk" cylinders (3 each at standard and modified compaction
#'   effort), with a 10% extra estimate to allow for PSA, Atterberg limits,
#'   and a margin for error.
#' @param w_final a numeric vector of the same length as `expt_mix_nums`
#'   (if the mixes are to have different water contents), or a single numeric
#'   value (if all mixes are to have the same final water content). Defaults to
#'   0.05 which is the lowest water content typically used in a compaction test.
#' @param sand_pct_in_sand a numeric vector of length 1 representing the
#'   fraction of the "sand" component which is >53 &mu;m sieve diameter, on an
#'   oven-dry mass basis (decimal form).
#' @param sand_pct_in_clay a numeric vector of length 1 representing the
#'   fraction of the "clay" component which is >53 &mu;m sieve diameter, on an
#' oven-dry mass basis (decimal form).
#' @param w_sand The gravimetric water content of the air-dry "sand" component,
#'   in decimal form.
#' @param w_clay The gravimetric water content of the air-dry "clay" component,
#'   in decimal form.
#'
#' @usage mix_calcs(mix_date, expt_mix_nums, sand_name, clay_name,
#'  final_sand_pcts, final_OD_kg= 43, w_final= 0.05, sand_pct_in_sand,
#'  sand_pct_in_clay, w_sand= 0.001, w_clay=0.02, backpack_flo_rate_g_per_sec= 28.3)
#'
#'
#'@return A ready-to-print table of values with an appropriate number of
#'  significant figures.
#'
#'@details
#'This function solves a 2-member system of equations, accounting for the
#'hygroscopic water content of each soil and their respective % sand-size
#'particles. The user may choose any desired final % sand, and (so long as the
#'water contents of each soil are known), the final mixture will contain the
#'desired % sand on an oven-dry mass basis.
#'
#'The equation for the air-dry mass of sandy soil is
#'
#' \mjdeqn{$m_{sandy~(air-dry)}~=~\frac{S_f~-~S_{clayey}}{S_{sandy}~-~S_{clayey}}~\cdot~(1+w_{sandy})~\cdot~m_{~total~mixture}$}{}
#'
#'and the equation for the
#'air-dry mass of clayey soil is
#'
#'\mjdeqn{
#'m_{clayey~(air-dry)}~=~\left\lbrack(1~-~\left(\frac{S_f~-~S_{clayey}}
#'{S_{sandy}~-~S_{clayey}}\right)\right\rbrack~\cdot~
#'(1~+~w_{clayey})~\cdot~m_{~total~mixture~(oven-dry)} }{}
#'
#'
#'@example inst/examples/mix_calcs_example.R
#'
#'@export
#'
#'

mix_calcs <- function(mix_date, expt_mix_nums, sand_name, clay_name,
                              final_sand_pcts, final_OD_kg= 43, w_final= 0.05,
                              sand_pct_in_sand, sand_pct_in_clay, w_sand= 0.001,
                              w_clay=0.02) {
  mix_ref <-   tibble::tibble(
    mix_date= lubridate::as_date(mix_date),
    expt_mix_nums = expt_mix_nums,
    sand_name = sand_name,
    clay_name = clay_name,
    sand_pct = final_sand_pcts,
    final_OD_kg = final_OD_kg,
    OD_sand_size_mass_in_final_mix = final_OD_kg*.data$sand_pct,
    OD_non_sand_size_mass_in_final_mix = final_OD_kg - .data$OD_sand_size_mass_in_final_mix,
    kg_OD_sand_component = ( final_OD_kg * (.data$sand_pct - sand_pct_in_clay) / (sand_pct_in_sand - sand_pct_in_clay) ),
    kg_OD_clay_component = final_OD_kg - .data$kg_OD_sand_component,
    kg_air_dry_sand_component = .data$kg_OD_sand_component*(1+w_sand),
    kg_air_dry_clay_component = .data$kg_OD_clay_component*(1+w_clay),
    kg_water_already_present = ( (w_sand * .data$kg_OD_sand_component) + (w_clay * .data$kg_OD_clay_component) ),
    kg_water_desired_after_mixing = w_final * final_OD_kg,
    kg_water_to_add = .data$kg_water_desired_after_mixing - .data$kg_water_already_present) %>%
    dplyr::select(mix_date, expt_mix_nums, sand_name, clay_name, .data$sand_pct,
                  .data$kg_air_dry_sand_component, .data$kg_air_dry_clay_component,
                  .data$kg_water_to_add) %>%
    dplyr::mutate(sand_pct= 100*.data$sand_pct,
                  kg_air_dry_sand_component= .data$kg_air_dry_sand_component,
                  kg_air_dry_clay_component = .data$kg_air_dry_clay_component)  %>%
    dplyr::rename(`Mix Date`=  mix_date,
                  `Mix number` = expt_mix_nums,
                  `Sand name` = sand_name,
                  `Clay name`= clay_name,
                  `Final % sand-size` = .data$sand_pct,
                  `kg sand component`= .data$kg_air_dry_sand_component,
                  `kg clay component`= .data$kg_air_dry_clay_component,
                  `kg water to add`= .data$kg_water_to_add)
  return(mix_ref)
}
