#'\lifecycle{experimental}
#'\loadmathjax
#'
#'Calculate physical properties of a soil specimen
#'
#'Generates all property values for every soil specimen in the data frame, i.e
#'each row. Output can be easily piped to `ggproctor()` for plotting purposes.
#'
#'@param df a data frame containing relevant specimen data, see **Details**
#'
#'@details The data passed in via `df` must contain specific gravity values (in
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
add_physical_properties <- function(df){

  # look up density of water at specified temperature

  water_density <- diRtscience::h2o_properties_w_temp_c %>%
    dplyr::filter(.data$water_temp_c == round(unique(df$ambient_temp_c), 1)) %>%
    .$water_density_Mg_m3 %>%
    .[1]

  newdf <- df %>%
    dplyr::mutate(
      moist_soil_g =.data$filled_cylinder_mass_g - .data$empty_cylinder_mass_g,
      OD_soil_g = .data$moist_soil_g / (1 + .data$water_content),
      moist_density = .data$moist_soil_g / .data$cylinder_vol_cm3,
      dry_density = .data$OD_soil_g / .data$cylinder_vol_cm3,
      total_porosity = 1 - (.data$dry_density / .data$Gs),
      void_ratio = 1 / (1 - .data$total_porosity),
      volumetric_water_content = (.data$water_content*.data$OD_soil_g/water_density) / .data$cylinder_vol_cm3,
      Se = (.data$water_content * .data$dry_density / water_density) / .data$total_porosity
    )

  return(newdf)
}
