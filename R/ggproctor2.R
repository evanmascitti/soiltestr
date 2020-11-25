#'
#' Plot a single compaction curve
#'
#' Fits a compaction curve using [proctor_fit()] and returns a **ggplot2** plot.
#' User may specify several annotations to the plot including the maximum
#' density, optimum water content, and zero air-voids curve. For plotting
#' multiple samples on the same plot, see `facet_ggproctor()`.
#'
#' @param df Raw laboratory data (masses and volumes)
#' @param points Show compaction points?
#' @param curve Show compaction curve
#' @param curve_se Display standard error around compaction curve?
#' @param spline_degree Number of knots to fit compaction curve, see
#'   [proctor_fit()]
#' @param label_d_max Label the maximum density?
#' @param label_w_opt Label the optimum water content?
#' @param guideline Draw a vertical line between &rho;~max~ and the x-axis?
#' @param zav Plot the zero air-voids curve?
#' @param zav_Gs Specific gravity used to compute the position of the ZAV curve;
#'   defaults to 2.7 but user should specify for better accuracy
#' @param set_labs Add formatted titles to axes?
#' @param point_col Color of points
#' @param point_size Size of points in mm
#' @param line_col Color of compaction curve trace
#' @param line_size Thickness of compaction curve trace in mm
#' @param ... Other arguments passed to layers comprising the plot
#'
#' @return A "gg" plot object
#' @export
#'
#' @example inst/examples/ggproctor_example.R
#'
ggproctor <- function(df = NULL,
                       points = TRUE,
                       curve = TRUE,
                       curve_se = FALSE,
                       spline_degree = 3,
                       label_d_max = TRUE,
                       label_w_opt = TRUE,
                       guideline = TRUE,
                       zav = TRUE,
                       zav_Gs = 2.7,
                       set_labs = TRUE,
                       point_col = "steelblue",
                       point_size = 2.5,
                       line_col = "grey20",
                       line_size = 0.5,
                       ...) {

  fit <- diRtscience::proctor_fit(df, spline_degree = spline_degree)

  points_data <- fit$physical_props

  w_opt_label_data <- tibble::tibble(
    w_opt = fit$w_opt[1],
    water_content = fit$w_opt[1],
    dry_density = min(points_data$dry_density) - 0.01,
    w_opt_label_text = paste("w[opt]==",
                             signif(w_opt[1], 3))
  )

  d_max_label_data <- tibble::tibble(
    d_max = fit$d_max[1],
    water_content = fit$w_opt[1],
    dry_density = fit$d_max[1] + 0.03,
    d_max_label_text = paste("bold(rho)[max]==",
                             signif(d_max, 3))
  )

  vert_guideline_data <- tibble::tibble(
    x = fit$w_opt[1],
    xend = fit$w_opt[1],
    y = -Inf,
    yend = fit$d_max - 0.005
  )

  zav_label_data <- tibble::tibble(
    water_content = 1.65 * fit$w_opt[1],
    dry_density = 0.9978 * zav_Gs / (1 + (zav_Gs * .data$water_content)),
    zav_label_text = "100%\nsaturation"
  )

  zav_line <-
    function(water_content)
      (0.9978 * zav_Gs) / (1 + (zav_Gs * water_content))

  zav_span <-
    max(points_data$water_content) - min(points_data$water_content)

  zav_min <- fit$w_opt[1] * 0.95

  zav_lims <- c(zav_min, 1.65 * fit$w_opt[1])


  # fudging some tick marks
  # ticks <- seq(#round(min(points_data$water_content), 2) - 0.03,
  #   0,
  #   round(1.65*fit$w_opt[1], 2) + 0.01,
  #   0.01)


  geom_proctor <- function() {
    list(

      # ggplot2::geom_vline(
      #   xintercept = ticks,
      #   color= "grey85"),

      if (zav) {
        list(
          ggplot2::geom_function(
            fun =  zav_line,
            color = "grey30",
            linetype = "dotted",
            xlim = zav_lims
          ),
          ggplot2::geom_label(
            data = zav_label_data,
            ggplot2::aes(label = .data$zav_label_text),
            size = 8 / .pt,
            color = "grey60",
            hjust = "inward",
            #vjust = "inward",
            nudge_y = -0.015,
            label.size = 0,

          )
        )
      },

      if (guideline) {
        ggplot2::geom_segment(
          data = vert_guideline_data,
          ggplot2::aes(
            x = .data$x,
            xend = .data$xend,
            y = .data$y,
            yend = .data$yend
          ),
          linetype = "dashed",
          color = line_col,
          size = 0.6
        )
      },

      if (label_w_opt) {
        ggplot2::geom_label(
          data = w_opt_label_data,
          ggplot2::aes(label = .data$w_opt_label_text),
          parse = TRUE,
          label.size = 0,
        )
      },

      if (label_d_max) {
        ggplot2::geom_label(
          data = d_max_label_data,
          ggplot2::aes(label = .data$d_max_label_text),
          parse = TRUE,
          label.size = 0
        )
      },

      if (set_labs)
      {
        ggplot2::labs(x = bquote("Water content, g g" ^ -1),
                      y = bquote("Dry density, Mg m" ^ -3))
      },

      if (curve) {
        ggplot2::geom_smooth(
          data = points_data,
          method = "lm",
          formula = y ~ splines::ns(x, spline_degree),
          se = curve_se,
          color = line_col,
          size = line_size,
          ...
        )
      },

      if (points) {
        ggplot2::geom_point(data = points_data,
                            color = point_col,
                            size = point_size,
                            ...)
      },

      ggplot2::scale_x_continuous(expand = ggplot2::expansion(
        mult = c(0, 0),
        add = c(0.03, 0.01)),
        breaks = scales::breaks_width(width = 0.05, offset = 0)),
      ggplot2::scale_y_continuous(expand = ggplot2::expansion(
        mult = c(0, 0),
        add = c(0.03, 0)),
        breaks = scales::breaks_width(width = 0.05, offset = 0)),
      cowplot::theme_cowplot(),
      cowplot::background_grid(major = "xy", minor = "x"),
      ggplot2::coord_fixed(ratio = .3185),
      ggplot2::theme(panel.grid.major = element_line(linetype = "dotted"),
                     panel.grid.minor = element_line(linetype = "dotted"))
    )
  }
  # end of geom_proctor

  # generate the actual plot object
  plot <- ggplot2::ggplot(data = points_data,
                          ggplot2::aes(.data$water_content,
                                       .data$dry_density,
                                       ...)) +
    geom_proctor()


  return(plot)

}
