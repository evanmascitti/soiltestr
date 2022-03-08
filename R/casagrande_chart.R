#' `r lifecycle::badge('maturing')`
#'
#' @title Plots the results of Atterberg limit tests on the standard Casagrande
#' plasticity chart
#'
#' @description Plots the U-line, A-line, and the classification symbols for
#' the regions on the chart.The water content is converted from decimal form to
#' percent form before plotting; therefore it should be in the data frame as a
#' decimal.
#'
#'
#' @param x a data frame containing LL and PI values
#' @import ggplot2
#' @return A `gg` plot object
#' @export
#'
casagrande_chart <- function(x) {

  plot1 <- ggplot(data= x, aes(100*.data$LL, 100*.data$PI))

  soil_class_labs <- tibble::tibble(soil_type= c('CL/OL', 'CH/OH', 'ML/OL', 'MH/OH', 'CL-ML'),
                            LL= c(40, 70, 40, 70, 16),
                            PI= c(21, 45, 6, 18, 5.5) )

  plot_to_output <-  plot1 +
    geom_point()+
    geom_segment(color= 'tomato', aes(x=4, xend=25.5, y=4, yend=4))+
    geom_segment(color= 'tomato', aes(x=25.5, xend=100, y=4, yend=0.73*(100 -20)))+
    geom_segment(color= 'darkblue', linetype= 'dotted', aes(x=16, xend=16, y=0, yend=7))+
    geom_segment(color= 'darkblue', linetype= 'dotted', aes(x=16, xend=73, y=7, yend= (0.9*(73-8)) ) ) +
    geom_segment(color='black', aes(x=50, xend= 50, y=0, yend=37.8) )+
    geom_segment(color= 'black', aes(x=0, y=0, xend= 0.73*(100 -20), yend= 0.73*(100 -20)))+
    coord_cartesian(xlim = c(0, 100),
                    ylim = c(0, 60),
                    expand = TRUE)+
    scale_x_continuous(breaks = seq(0, 100, 20)) +
    geom_text(data = soil_class_labs, inherit.aes= FALSE, aes(.data$LL, .data$PI, label= .data$soil_type), size=3, fontface= 'bold')+
    annotate('text', x= 65, y= 0.9*(65-8)+3, label= 'U-line', color= 'darkblue', size=3, angle= 43, hjust=0.9)+
    annotate('text', x= 90, y= 0.73*(90 -20)+3, label= 'A-line', color= 'tomato', size=3, angle= 37, hjust=0.9)+
    annotate('text', x= 50, y= 50+3, label= '1:1 slope', color= 'black', size=3, angle= 45, hjust=0.9)+
    geom_polygon(data= tibble::tibble(LL=c(4, 7, 20+(7/0.73), 25.5),
                              PI=c(4, 7, 7, 4)),
                 inherit.aes= FALSE,
                 aes(.data$LL, .data$PI),
                 fill='grey',
                 alpha=1/3) +
    labs(x='LL', y='PI')+
    theme_classic()+
    cowplot::background_grid(minor = 'xy')+
    theme(axis.title.y = element_text(angle=0, vjust=0.5),
          plot.background  = element_rect(color='black', fill= 'transparent', size=0.25),
          panel.background = element_rect(color= NA, fill= 'transparent'))
  plot_to_output

}
