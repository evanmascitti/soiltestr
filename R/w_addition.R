#' Compute water addition for an air-dry soil
#'
#' @param OD_mass Double. Oven-dry mass of soil in g or kg
#' @param w_extant Double. Existing water content (decimal).
#' @param w_final Double. Desired final water content (decimal)
#'
#' @return A tibble with one row and two columns - the air-dry mass of soil to use and the mass of water to add.
#' @export
#'
#' @example inst/examples/w_addition_example.R
#'
w_addition <- function(OD_mass, w_extant, w_final){

  existing_water <- OD_mass * w_extant

  final_water <- OD_mass * w_final

  air_dry_soil_to_use <- OD_mass + w_extant * existing_water

  water_to_add <- final_water - existing_water

  return_tbl <- tibble::tibble(
    air_dry_soil_to_use = air_dry_soil_to_use,
    water_to_add = water_to_add
  )

  return(return_tbl)

}
