library(tidyverse)
library(latex2exp)
library(diRtscience)

data_vals <- diRtscience::example_proctor_data %>%
  filter(compaction_effort == "standard") %>%
  proctor_fit() %>%
  .$physical_props

proctor_plot <- function(df, w = water_content, rho = dry_density,
                         se = FALSE, line_color = NULL, spline_degree = 3,
                         Gs= 2.70){

  zav <- function(water_content) (0.9978*Gs) / ( 1 + (Gs*water_content) )

  p <- ggplot2::ggplot(data= df,
                       ggplot2::aes(x= {{w}}, y= {{rho}}))+
    ggplot2::geom_point()+
    ggplot2::geom_smooth(method = "lm",
                         formula = y~splines::ns(x, spline_degree),
                         se = se,
                         aes(color= line_color ) )+
   #  browser()
    ggplot2::scale_x_continuous(breaks = scales::breaks_width(0.05),
                                limits = c(0,0.3),
                                #expand = c(0.1, 0.1),
                                #minor_breaks = scales::minor_breaks_n(5),
                                labels = scales::label_number(accuracy = 0.01),
                                name= latex2exp::TeX("Water content, g/g") )+
    ggplot2::scale_y_continuous(breaks = scales::breaks_width(width= 0.1),
                                name= latex2exp::TeX("Dry density, Mg/m$$^3$$"))+
    ggplot2::geom_function(fun =  zav, color=grey.colors(10) %>% .[5], linetype="dashed")+
    ggplot2::coord_fixed(ratio = 0.32 )+ # 2 lb/ft3 per 1% in water content
    cowplot::theme_cowplot()+
    cowplot::background_grid()+
    labs(title = "Proctor compaction curve")
  return(p)
  }
standard_eff_plot <- example_proctor_data %>%
  filter(compaction_effort== "standard") %>%
  proctor_fit(Gs=2.71) %>%
  .$physical_props %>%
  proctor_plot(Gs = 2.71)

modified_eff_plot <- example_proctor_data %>%
  filter(compaction_effort== "modified") %>%
  proctor_fit(Gs = 2.71) %>%
  .$physical_props %>%
  proctor_plot(Gs = 2.71, line_color = "red")


cowplot::plot_grid(standard_eff_plot, modified_eff_plot)
