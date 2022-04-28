#'\lifecycle{experimental}
#'\loadmathjax
#'
#' @title Calculate physical properties of a soil specimen
#'
#'@description Generates all property values for every soil specimen in the
#'data frame, i.e each row. Output can be easily piped to `ggproctor()` for
#'plotting purposes.
#'
#' @param x a data frame containing relevant specimen data, see **Details**
#' @param mold_dimensions data frame containing metadata about molds used in tests. If not supplied, looks for global option `soiltestr.proctor_molds`.
#' @param cleat_mark Logical. Is this analysis for cleat-mark cylinders?
#'
#'@details The data passed in via `x` must contain specific gravity values (in
#'  a column named `Gs`), gravimetric water contents in a column named
#'  `water_content`, and the ambient test temperature (&deg;C) in a column named
#'  `ambient_temp_c`. Water contents can be easily computed with [`add_w()`].
#'  The use of [`generate_proctor_datasheet()`] ensures compatibility of the the
#'  column names used during data collection with those expected by this
#'  function.
#'
#'  The properties added to the data frame include moist and oven-dry soil mass
#'  in grams, moist and dry density (&rho;) in
#'  g/cm\ifelse{html}{\out{<sup>3</sup>}}{\eqn{^3}{^3}}, total porosity (n),
#'  void ratio (e), and effective saturation (Se).
#'
#'@return mutated data frame with new columns, see details
#'@export
#'
add_physical_properties <- function(x, mold_dimensions = NULL, cleat_mark = FALSE){

  # look up density of water at specified temperature

  # water_density <- soiltestr::h2o_properties_w_temp_c %>%
  #   dplyr::filter(.data$water_temp_c == round(unique(x$ambient_temp_c), 1)) %>%
  #   .$water_density_Mg_m3 %>%
  #   .[1]


  # find proctor cylinders lookup


  mold_dimensions <- mold_dimensions %||% getOption('soiltestr.proctor_molds') %||% internal_data$equipment_instructions("proctor_molds")


 # browser()

  # add a line to re-name the columns from a cleat-mark cylinder test
  # into names compatible with this function. They mean the same thing,
  # but the names in the chunk cyl dims object are different to reflect
  # the fact that they also account for the mass and volume of the
  # resin plug in the bottom

  # In this special case, the `cylinder_ID` column in the
  # mold_dimensions data frame also needs to be re-named.

  if(cleat_mark){

    x <- dplyr::rename_with(
      x,
      .fn = ~stringr::str_replace(
        string = .,
        pattern = "cylinder",
        replacement = "mold"
    ))

  mold_dimensions <- dplyr::rename(
    mold_dimensions,
    mold_ID = cylinder_ID,
    empty_mold_mass_g = cyl_w_plug_mass,
    mold_vol_cm3 = cyl_w_plug_volume
  )

  }

# add assumed Gs value if data frame does not already contain one

if(!"Gs" %in% names(x)){
  x$Gs <- 2.7
  warning("No Gs value specified in data frame. Defaulting to 2.7.", call. = FALSE)
}


#   browser()

  # perform calculations on the data frame
  # join with mold dimensions data frame


  # browser()

  newdf <- x %>%
    dplyr::left_join(mold_dimensions, by = "mold_ID") %>%
    dplyr::left_join(
      h2o_properties_w_temp_c,
      by = c("ambient_temp_c" = "water_temp_c")
    ) %>%
    dplyr::mutate(
      moist_soil_g =.data$filled_mold_g - .data$empty_mold_mass_g,
      OD_soil_g = .data$moist_soil_g / (1 + .data$water_content),
      moist_density = .data$moist_soil_g / .data$mold_vol_cm3,
      dry_density = .data$OD_soil_g / .data$mold_vol_cm3,
      total_porosity = 1 - (.data$dry_density / .data$Gs),
      void_ratio = 1 / (1 - .data$total_porosity),
      volumetric_water_content = (.data$water_content*.data$OD_soil_g/.data$water_density_Mg_m3) / .data$mold_vol_cm3,
      Se = (.data$water_content * .data$dry_density / .data$water_density_Mg_m3) / .data$total_porosity
    ) %>%
    dplyr::select(-c(.data$water_absolute_viscosity_poises,
                       .data$water_density_Mg_m3,
                       .data$water_density_kg_m3
                       ))

  return(newdf)
}
