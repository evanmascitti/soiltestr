#' Fit a compaction curve
#'
#' The compaction curve is fit using a natural cubic spline. The function
#' returns a 5-item list.  Porosity is calculated using a default specific
#' gravity value of 2.7, but this must be over-ridden with a measured \eqn{G_s} value
#' if better control is required. The other 4 items in the list are the model
#' object, the object converted to a  function, the optimum water content for
#' the present compaction effort, and the maximum dry density for the present
#' compaction effort. The values of \eqn{w_{opt}} and \eqn{d_{max}} are computed by an
#' optimization of the the spline function.
#'
#'
#' @param df A data frame of raw compaction data containing (at a
#'   minimum) columns named "water_content", "filled_cylinder_mass_g",
#'   "empty_cylinder_mass_g", and " cylinder_vol_cm3"
#'
#' @param spline.degree The number of terms in each piecewise polynomial
#'   spline (defaults to 3). May not exceed more than n-1 where n is the number
#'   of compaction cylinders tested.
#'
#' @param Gs The specific gravity of the soil solids, used to calculate
#'    total porosity. The default value of 2.7 may be used as this is typical
#'    for sand-clay mixes. If better control is desired (i.e. for publication
#'    rather than a "first glance"; in this case it is strongly suggested that a
#'    measured pycnometer value is used.)
#'
#' @return \describe{
#'    \strong{physical_props }{A data frame containing the cylinder number, water content, moist density, dry density, total porosity, and void ratio for each compaction cylinder}
#'
#'    \strong{proctor_model }{Model object of class "lm"}
#'
#'    \strong{proctor_function }{A function derived from proctor_model}
#'
#'    \strong{w_opt }{The optimum water content for the present compaction effort}
#'
#'    \strong{d_max }{The maximum dry density for the present compaction effort}
#'    }
#'
#' @export
#'
#'@references Proctor, 1932. Description of field and laboratory methods. \emph{Eng. News Record}, 110:10, p. 286-289.
#'
#'@references Standard effort: \href{https://www.astm.org/Standards/D698.htm}{ASTM D698-12e2}
#'
#'@references Modified effort: \href{https://www.astm.org/Standards/D1557}{ASTM D1557-12e1}
#'
#'
proctor_fit <- function(df, spline.degree=3, Gs= 2.70) {
  # first compute water content, oven-dry soil mass, moist density, and dry density
  physical_props <- df %>%
    mutate(OD_soil_g = (filled_cylinder_mass_g - empty_cylinder_mass_g) / (1+ water_content ),
           moist_density= (filled_cylinder_mass_g - empty_cylinder_mass_g) / cylinder_vol_cm3,
           dry_density= OD_soil_g / cylinder_vol_cm3) %>%
    select(cylinder_num, water_content, moist_density, dry_density) %>%
    mutate(total_porosity= 1-(dry_density / Gs),
           void_ratio= 1/ (1+total_porosity)
    ) %>%
    as.data.frame()

  proctor_model <- na.omit(lm(data=physical_props, formula = dry_density ~ splines::ns(water_content, spline.degree ) ) )

  proctor_function <- mosaic::makeFun(object= proctor_model)

  w_opt <- optimize(f= proctor_function,
                    interval = c(min(df$water_content, na.rm = TRUE),
                                 max(df$water_content, na.rm = TRUE) ),
                    maximum = TRUE) %>%
    .$maximum %>%
    .[1]
  d_max <- optimize(f= proctor_function,
                    interval = c(min(df$water_content, na.rm = TRUE),
                                 max(df$water_content, na.rm = TRUE) ),
                    maximum = TRUE) %>%
    .$objective %>%
    .[[1]]

  return(list(physical_props= physical_props,
              proctor_model= proctor_model,
              proctor_function = proctor_function,
              w_opt = w_opt,
              d_max = d_max)
  )
}
