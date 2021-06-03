#' \lifecycle{experimental}
#'
#' @title Plot the flow curve from one or more liquid limit tests
#'
#' @description The water content is plotted on the y-axis with a linear scale
#'   while the blow count is plotted on the x-axis with a natural log scale. The
#'   liquid limit is computed as the water content at which a groove cut through
#'   the soil specimen closes over a width of 12.5 mm after 25 blows. The liquid
#'   can be computed with [`compute_LL()`].
#'
#' @param df a data frame containing named columns `water_content()` and `blow_count()`
#' @param ... additional aesthetic mappings passed onto `geom_point()` and `geom_smooth()`.
#'
#' @seealso [`add_w()`], [`compute_LL()`]
#'
#' @return a 'gg' plot object
#' @export
#'
#' @example /inst/examples/ggflowcurve_example.R
ggflowcurve <- function(df, ...){


  plot <- ggplot2::ggplot(data= df, aes(.data$blow_count, .data$water_content))+
    ggplot2::geom_vline(xintercept = 15:35, color = 'grey95', size = 0.25)+
    ggplot2::geom_smooth(aes(...),
                         formula = y~x,
                         method = "lm",
                         se= F,
                         size = 0.25)+
    ggplot2::geom_point(aes(...), alpha = 1/3)+
    ggplot2::scale_x_continuous(
      name= 'Blow count, log scale',
      trans = 'log',
      limits = c(15, 35),
      breaks = seq(15, 45, 10)
      )+
    ggplot2::scale_y_continuous(
      name = expression('Water content, g g'^-1 %*% 100),
      labels = scales::label_percent(accuracy = 0.1, suffix = ""),
      breaks = scales::breaks_width(width = 0.002),
      # n.breaks = 6,
      expand = ggplot2::expansion(mult = 0.25))+
    ggplot2::ggtitle(
      label = 'Liquid limit test data',
      subtitle = 'Casagrande cup, ASTM D4318')+
    ggplot2::theme_classic()+
    cowplot::background_grid(size.major = 0.25, size.minor = 0.25)+
    ggplot2::theme(
      panel.grid.minor = ggplot2::element_line(linetype = 'dotted'))

  return(plot)
}
