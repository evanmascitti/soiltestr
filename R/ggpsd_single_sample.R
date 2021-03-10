#' @title Plot a single particle size distribution
#'
#' @description Displays the cumulative percent of sample passing a range of sizes.
#'
#' @param df Data frame containing columns titled `microns` and `percent_passing`
#' @details This is a special case of [`ggpsd()`] for plotting only a single
#'   sample. The plot subtitle is programmed to supply additional information
#'   about the test.
#' @param ... Other arguments passed on to `aes()` in the initial call to `ggplot()` (affects both points and lines)
#'
#' @return a ggplot object
#' @export
#'
ggpsd_single_sample <- function(df, ...){


  log_lines <- c(seq(0.1, 1, 0.1),
                 seq(1, 10, 1),
                 seq(10, 100, 10),
                 seq(100, 1000, 100),
                 seq(1000, 10000, 1000))

  bold_log_lines <- c(0.1, 1, 10, 100, 1000, 10000)

  df %>%
    ggplot2::ggplot(ggplot2::aes(x = .data$microns, y = .data$percent_passing, ...))+
    ggplot2::scale_x_continuous("Particle diameter, \u03bcm",
                                limits = c(0.1, 10000),
                                trans = "log10",
                                breaks = c(0.1, 1, 10, 100, 1000, 10000),
                                labels = c(0.1, 1, 10, 100, 1000, 10000))+
    ggplot2:: scale_y_continuous(bquote("% passing, g g"^-1),
                                 limits = c(0, 1),
                                 breaks = seq(0, 1, 0.2),
                                 labels = scales::label_percent(suffix = ""))+
    ggplot2::geom_vline(xintercept = log_lines, color = 'grey80', linetype = 'dotted')+
    ggplot2::geom_vline(xintercept = bold_log_lines, color= 'grey80')+
    ggplot2::geom_hline(yintercept = seq(0, 1, 0.2), color = 'grey80', linetype = 'dotted')+
    ggplot2::geom_point(alpha = 2/3, size = 2)+
    ggplot2::geom_line()+
    ggplot2::ggtitle("Cumulative particle size distribution",
                     subtitle = paste("Sample name:",
                                      unique(df$sample_name),
                                      "(replicate #",
                                      unique(df$replication),
                                      "). Tested",
                                      unique(df$date),
                                      "."))+
    cowplot::theme_cowplot()+
    ggplot2::theme(
      axis.line.y = element_blank(),
      axis.ticks.y = element_blank(),
      plot.title = element_text(hjust = 0.5)
    )
  }

