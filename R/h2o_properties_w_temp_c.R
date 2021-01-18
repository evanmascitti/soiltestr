#' Variation in density and viscosity of pure water with temperature
#'
#' These parameters are required for specific gravity, bulk density, and
#' particle size analyses. This data set provides a convenient look-up table for
#' internal functions in **soiltestr** or for the user's own computations.
#'
#' @source [ASTM D854 - 14](https://www.astm.org/Standards/D854),
#' [Engineer's edge](https://www.engineersedge.com/physics/water__density_viscosity_specific_weight_13146.htm)
#'
#' @format A data frame with 5 columns (temperature in &deg;^C^ and Kelvin; absolute
#'   viscosity, and density in two units (Mg m^-3^ and kg m^-3^; the former is
#'   the soil science standard unit; the latter uses SI base units).
#'
"h2o_properties_w_temp_c"
