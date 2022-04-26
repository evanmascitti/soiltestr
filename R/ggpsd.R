#' @title Plot one or more particle size distributions
#'
#' @description Displays the cumulative percent of sample passing a range of
#'   sizes.
#'
#' @param df Data frame containing columns titled `microns` and
#'   `percent_passing`
#' @param units Whether x-axis should be displayed in &mu;cm or mm
#' @param points Whether to include points on the plot (default is TRUE, set to FALSE for lines only)
#' @param log_lines Logical. Plot the canonical log-10 vertical grid?
#' @param bold_log_lines Logical. Plot thicker lines at 0.1, 1, 10, 100, and 1000 microns
#' @param ... Other arguments passed on to `aes()` in the initial call to
#'   `ggplot()` (affects both points and lines)
#' @details This function is designed for plotting multiple specimens via the
#'   `(...)` argument passed internally to `aes()`. See
#'   [`ggpsd_single_sample()`] for plotting only a single sample. That function
#'   automatically annotates the plot subtitle with additional information about
#'   the test.
#'
#' @return a ggplot object
#' @export
#'
#'


# main function (see `gg_psdpts()` for helper function)

ggpsd <- function(df, units = "microns", points = TRUE, log_lines = TRUE, bold_log_lines = TRUE, ...){


  units <- match.arg(units, choices = c("microns", "mm"))



  log_line_breaks <- c(seq(0.1, 1, 0.1),
                 seq(1, 10, 1),
                 seq(10, 100, 10),
                 seq(100, 1000, 100),
                 seq(2000, 4000, 1000))


  psa_log_lines <- function(){
    if(log_lines){
      list(
        ggplot2::geom_vline(xintercept = log_line_breaks,
                            color = 'grey95',
                            linetype = 'dotted',
                            size = 0.3)
      )
    }
  }

  bold_log_line_breaks <- c(0.1, 1, 10, 100, 1000)

  psa_bold_log_lines <- function(){
    if(bold_log_lines){
      list(
        ggplot2::geom_vline(xintercept = bold_log_line_breaks,
                            color= 'grey90',
                            size = 0.3)
      )
    }
  }

  psa_points <- function(){

    if(points){
      list(
      ggplot2::geom_point(alpha = 1/2)
      )
    }
  }


  if(units == "microns"){
  psd_plot <- df %>%
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
    psa_log_lines()+
    psa_bold_log_lines()+
    ggplot2::geom_hline(yintercept = seq(0, 1, 0.2), color = 'grey90',
                        size = 0.25)+
    ggplot2::geom_line( alpha = 1/2)+
    psa_points()+
    ggplot2::ggtitle("Cumulative particle size distribution")+
    ggplot2::theme_classic()+
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

  if(units == "mm"){

    psd_plot <- df %>%
      ggplot2::ggplot(ggplot2::aes(x = .data$microns / 1000, y = .data$percent_passing, ...))+
      ggplot2::scale_x_continuous(expression("Particle diameter, mm"),
                                  limits = 0.001 * c(0.1, 4000),
                                  trans = "log10",
                                  breaks = 0.001 * c(0.1, 1, 10, 100, 1000),
                                  labels = scales::comma(0.001 * c(0.1, 1, 10, 100, 1000)))+
      ggplot2:: scale_y_continuous("% passing",
                                   limits = c(0, 1),
                                   breaks = seq(0, 1, 0.2),
                                   labels = scales::label_percent(suffix = ""))+
      psa_log_lines()+
      psa_bold_log_lines()+
      ggplot2::geom_hline(yintercept = seq(0, 1, 0.2), color = 'grey90',
                          size = 0.25)+
      ggplot2::geom_line( alpha = 1/2)+
      psa_points()+
      ggplot2::ggtitle("Cumulative particle size distribution")+
      ggplot2::theme_classic()+
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

  return(psd_plot)


  }

