#'Fit a compaction curve
#'
#'\lifecycle{maturing}
#'\loadmathjax
#'The compaction curve is fit using a natural
#'cubic spline and optimized to find \eqn{D_{max}} and \eqn{w_{opt}}.
#'
#'@param df A tibble containing raw compaction data containing (at a minimum)
#'  columns \code{"water_content", "filled_cylinder_mass_g",
#'  "empty_cylinder_mass_g", "cylinder_vol_cm3"}
#'@param spline_degree The number of terms in each piece-wise polynomial spline
#'  (defaults to 3). May not exceed more than n-1 where n is the number of
#'  compaction cylinders tested.
#'
#'@return A list with 5 elements:
#'
#'- `physical_props` a tibble containing the cylinder number, the water content,
#'and the computed physical properties for each specimen:
#'  - moist density
#'  - dry density
#'  - total porosity
#'  - void ratio
#'  - effective saturation
#'
#'Note: porosity is calculated using the specific gravity contained in
#'  the required `Gs` column of the data frame passed via `df`.
#'
#'- `proctor_model` S3 object returned by a call to `lm`
#'
#'- `proctor_function` R bytecode function derived from proctor_model
#'
#'- `w_opt` The optimum water content for the present compaction effort
#'
#'- `d_max` The maximum dry density for the present compaction effort
#'
#'@details The values of \mjseqn{w_{opt}} and \mjseqn{\rho_{max}} are computed
#'  by an optimization of the the spline function, see [`stats::optimize()`].
#'
#' @export
#'
#'@references Proctor, 1932. Description of field and laboratory methods. _Eng.
#'  News Record-, 110:10, p. 286-289.
#'
#'@references Standard effort: \href{https://www.astm.org/Standards/D698.htm}{ASTM D698-12e2}
#'
#'@references Modified effort: \href{https://www.astm.org/Standards/D1557}{ASTM D1557-12e1}
#'
#'@seealso [`generate_proctor_datasheet()`], [`proctor_prep()`], [`mix_calcs()`], [`stats::optimize()`]
#'
#'@example inst/examples/proctor_fit_example.R
#'
proctor_fit <- function(df,
           spline_degree = 3) {

  # stop if any packages are missing
  ecmfuns::pkg_check(c("mosaic", "splines"))

  physical_props <- df %>%
    dplyr::select(.data$cylinder_number,
                  .data$water_content,
                  .data$moist_density,
                  .data$dry_density,
                  .data$total_porosity,
                  .data$void_ratio,
                  .data$volumetric_water_content,
                  .data$Se,
                  .data$Gs,
                  .data$moist_soil_g,
                  .data$OD_soil_g,)

    proctor_model <-
      stats::na.omit(stats::lm(
        data = df,
        formula = dry_density ~ splines::ns(water_content, spline_degree)
      ))

    proctor_function <- mosaic::makeFun(object = proctor_model)

    w_opt <- stats::optimize(f = proctor_function,
                             interval = c(
                               min(df$water_content, na.rm = TRUE),
                               max(df$water_content, na.rm = TRUE)
                             ),
                             maximum = TRUE) %>%
      .$maximum %>%
      .[1]

    d_max <- stats::optimize(f = proctor_function,
                             interval = c(
                               min(df$water_content, na.rm = TRUE),
                               max(df$water_content, na.rm = TRUE)
                             ),
                             maximum = TRUE) %>%
      .$objective %>%
      .[[1]]

    return(
      list(
        physical_props = physical_props,
        proctor_model = proctor_model,
        proctor_function = proctor_function,
        w_opt = w_opt,
        d_max = d_max
      )
    )
  }

