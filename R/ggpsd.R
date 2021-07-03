#' @title Plot one or more particle size distributions
#'
#' @description Displays the cumulative percent of sample passing a range of
#'   sizes.
#'
#' @param df Data frame containing columns titled `microns` and
#'   `percent_passing`
#'@param points Whether to include points on the plot (default is TRUE, set to FALSE for lines only)
#' @details This function is designed for plotting multiple specimens via the
#'   `(...)` argument passed internally to `aes()`. See
#'   [`ggpsd_single_sample()`] for plotting only a single sample. That function
#'   automatically annotates the plot subtitle with additional information about
#'   the test.
#' @param ... Other arguments passed on to `aes()` in the initial call to
#'   `ggplot()` (affects both points and lines)
#'
#' @return a ggplot object
#' @export
#'
#'


# main function (see `gg_psdpts()` for helper function)

ggpsd <- function(df, points = TRUE, ...){


  log_lines <- c(seq(0.1, 1, 0.1),
                 seq(1, 10, 1),
                 seq(10, 100, 10),
                 seq(100, 1000, 100),
                 seq(2000, 4000, 1000))

  bold_log_lines <- c(0.1, 1, 10, 100, 1000)

  df %>%
    ggplot2::ggplot(ggplot2::aes(x = .data$microns, y = .data$percent_passing, ...))+
    ggplot2::scale_x_continuous(expression("Particle diameter, \u03bcm"),
                                limits = c(0.1, 4000),
                                trans = "log10",
                                breaks = c(0.1, 1, 10, 100, 1000),
                                labels = as.character(c(0.1, 1, 10, 100, "1,000")))+
    ggplot2:: scale_y_continuous("% passing",
                                 limits = c(0, 1),
                                 breaks = seq(0, 1, 0.2),
                                 labels = scales::label_percent(suffix = ""))+
    ggplot2::geom_vline(xintercept = log_lines,
                        color = 'grey96',
                        linetype = 'dotted',
                        size = 0.3)+
    ggplot2::geom_vline(xintercept = bold_log_lines,
                        color= 'grey96',
                        size = 0.3)+
    ggplot2::geom_hline(yintercept = seq(0, 1, 0.2), color = 'grey90',
                        size = 0.25)+
    ggplot2::geom_point(alpha = 1/2)+
    ggplot2::geom_line( alpha = 1/2)+
    ggplot2::ggtitle("Cumulative particle size distribution")+
    cowplot::theme_cowplot()+
    ggplot2::theme(
      axis.line.y = ggplot2::element_blank(),
      axis.ticks.y = ggplot2::element_blank(),
      #axis.text.x = ggplot2::element_text(angle = 45),
      axis.text = ggplot2::element_text(size = 10, color = 'grey50'),
      axis.title = ggplot2::element_text(color = 'grey25'),
      axis.line.x = ggplot2::element_line(color = 'grey50'),
      axis.ticks.x = ggplot2::element_line(color = 'grey50'),
      plot.title = ggplot2::element_text(hjust = 0.5, face = 'plain')
    )
  }

