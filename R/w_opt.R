#'Fit a compaction curve, returning optimum water content only
#'
#'\lifecycle{stable} \loadmathjax This is a convenient wrapper around
#'[proctor_fit()] which returns only the **optimum water content** (at the effort used), derived from the compaction
#'curve. This is useful when the user wants to perform a "quick-and-dirty"
#'analysis and avoid additional indexing into the more verbose result of
#'`proctor_fit()`.
#'
#'The compaction curve is fit using a natural spline, with the spline degree set
#'by the user (defaults to cubic). Porosity is calculated using a default
#'specific gravity value of 2.7, but this must be over-ridden with a measured
#'\mjseqn{G_s} value if better control is required. The \mjseqn{w_{opt}} is
#'computed by an optimization of the the spline function.

#'
#'@param df A tibble containing raw compaction data containing (at a minimum)
#'  columns \code{"water_content", "filled_cylinder_mass_g",
#'  "empty_cylinder_mass_g", "cylinder_vol_cm3"}
#'@param ... Other arguments passed on to [`proctor_fit()`]
#'
#'@return {w_opt }{The optimum water content for the present compaction effort}
#'
#'@export
#'
#'@references Proctor, 1932. Description of field and laboratory methods.
#'  \emph{Eng. News Record}, 110:10, p. 286-289.
#'
#'@references Standard effort:
#'  \href{https://www.astm.org/Standards/D698.htm}{ASTM D698-12e2}
#'
#'@references Modified effort: \href{https://www.astm.org/Standards/D1557}{ASTM
#'  D1557-12e1}
#'
#'@seealso [proctor_fit()], [d_max()]
#'
# inst/examples/w_opt_example.R
#'
w_opt <- function(df, ...) {

  df %>%
    proctor_fit(...) %>%
    .[["w_opt"]]
}
