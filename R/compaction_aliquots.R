#'Simplify the process of adding water to soil compaction specimens
#'
#'\lifecycle{experimental} \loadmathjax When preparing soil for compaction
#'testing, it is often desired to mix the specimens to known water contents,
#'rather than "eyeballing" the water additions. This ensures the water contents
#'of the test specimens tested will bracket the optimum water content and be
#'spaced along a roughly equal range. Some \emph{a priori} knowledge about the
#'soil is needed to predict what this range will be. The function allows the
#'user to specify the range of water contents that should be obtained for the
#'test along with the anticipated maximum density, and it returns information
#'about the mass of soil to prepare and how much water to add to each specimen.
#'
#'Using a battery-powered spray bottle increases the uniformity of water
#'throughout the soil, and improves the efficiency of the process. The
#'\code{spray_flo_rate} argument allows the user to specify the flow rate of the
#'bottle, if used.
#'
#'Many tests are often performed in the same day, so the function is designed to
#'make the calculations for multiple soils at multiple compaction efforts.
#'
#'@param expt_mix_nums A vector of the unique identifiers for the soils being
#'  prepared
#'@param n_cylinders The number of compaction points to prepare for each soil
#'@param effort A character vector containing the names of the compaction
#'  methods to use
#'@param w_extant The current water content of the soil
#'@param standard_w_range The full range of water contents to test with the
#'  standard effort
#'@param modified_w_range The full range of water contents to test with the
#'  modified effort
#'@param assumed_dry_density A conservatively high estimate of the maximum dry
#'  density achievable for this soil with the modified effort. Used to determine
#'  the aliquot mass for each compaction point.
#'@param cylinder_volume The volume of the compaction mold, defaults to 937.4
#'  cm\mjeqn{^3}{}. Should be calibrated by the user for their own apparatus
#'  (see \href{https://www.astm.org/Standards/D698.htm}{ASTM D698-12e2} Annex A1
#'  for protocol)
#'@param spray_flo_rate The flow rate of water being sprayed on the mixture, in
#'  cm/s
#'
#'@return A tibble containing information about the specimens to prepare, most
#'  critically the initial moist mass to use and the time each specimen should
#'  be sprayed to bring it the desired water content.
#'
#'@example R/examples/compaction_aliquots_example.R
#'
#'@seealso [proctor_fit]
#'
#' @export
#'
#'@references \href{https://www.astm.org/Standards/D698.htm}{ASTM D698-12e2}

compaction_aliquots <- function(expt_mix_nums, n_cylinders=5, effort= c("standard", "modified"),
                                w_extant=0.05, standard_w_range=c(0.08, 0.14),
                                modified_w_range=c(0.05, 0.11), assumed_dry_density=2.235,
                                cylinder_volume=937.4, spray_flo_rate= 2.40)

  {

  n_mixes <-  length(unique(expt_mix_nums))

  standard_w <-  seq(min(standard_w_range), max(standard_w_range), length.out = n_cylinders)

  modified_w <-  seq(min(modified_w_range), max(modified_w_range), length.out = n_cylinders)

  aliquot_masses <- tibble::tibble(expt_mix_num= rep(expt_mix_nums, each= n_cylinders*length(unique(effort))) ,
                                   effort= rep(rep(effort, each=n_cylinders), times= n_mixes),
                                   w_extant= w_extant,
                                   w_desired= rep(c(standard_w, modified_w), times=n_mixes),
                                   cylinder_num = rep(rep(c(1:n_cylinders), times= length(unique(effort))),
                                                      times= n_mixes),
                                   dry_density= assumed_dry_density,
                                   cylinder_volcm3= cylinder_volume,
                                   OD_mass = .data$dry_density*.data$cylinder_volcm3,
                                   moist_aliquot_mass= round(.data$OD_mass*(1+w_extant), 0),
                                   w_to_add_g= (.data$w_desired - .data$w_extant)*.data$OD_mass,
                                   sec_to_spray= round(.data$w_to_add_g/spray_flo_rate, 0),
                                   sec_to_spray_period= lubridate::as.period(lubridate::as.duration(.data$sec_to_spray)),
                                   time_to_spray= sprintf('%02d:%02d', lubridate::minute(.data$sec_to_spray_period), lubridate::second(.data$sec_to_spray_period))
  ) %>%
    dplyr::select(.data$expt_mix_num, .data$effort, w_extant, .data$w_desired, .data$cylinder_num,
                  .data$moist_aliquot_mass, .data$w_to_add_g, .data$time_to_spray)

  aliquot_masses

}
