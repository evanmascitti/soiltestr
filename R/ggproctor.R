#' \lifecycle{experimental}
#'
#' @title Plot one or more compaction curves
#'
#' @description Fits a compaction curve using a natural cubic spline and returns a
#' **ggplot2** plot. The maximum dry density and optimum water content can be
#' annotated on the plot and the user may specify whether the 90% and 100%
#' satration curves should be drawn. Faceting and the addition of other
#' **ggplot** layers are supported.
#'
#' @param df data frame containing water contents and dry densities
#' @param identifier unquoted column name which distinguishes the samples from each other, defaults to `sample_name`
#' @param annotate whether to print the values of maximum density and optimum water content on the plot
#' @param sat_100 display the 100% saturation line (temperature of 22 &deg;^C^ assumed)
#' @param sat_90 display the 90% saturation line (temperature of 22 &deg;^C^ assumed)
#' @param ... other arguments passed on to `geom_smooth` and `geom_point()`
#'
#' @return a 'gg' plot object
#' @export
#'
#' @example /inst/examples/ggproctor_example.R
#'

ggproctor <- function(df, identifier = sample_name,
                       annotate = TRUE, sat_100 = TRUE,
                       sat_90 = TRUE, ...){


  # stop if any packages are missing
  ecmfuns::pkg_check(c("mosaic", "splines"))


  # error if plotting soils with different Gs values and not using faceting


  # make new data frame and then
  # write function for annotating d_max and w_opt

  annotation_df <- suppressMessages( df %>%
    dplyr::group_by({{identifier}}) %>%
    tidyr::nest() %>%
    dplyr::mutate(
      d_max = purrr::map_dbl(.data$data, soiltestr::d_max),
      w_opt = purrr::map_dbl(.data$data, soiltestr::w_opt),
      d_max_label_text = paste(
        "bold(rho)[max]==", round(.data$d_max, 2)),
      w_opt_label_text = paste(
        "w[opt]==", round(.data$w_opt, 3))
    ) %>%
    dplyr::ungroup() %>%
    dplyr::select(
      {{identifier}},
      .data$d_max,
      .data$w_opt,
      .data$d_max_label_text,
      .data$w_opt_label_text  )
  )

  geom_proctor_annotation <- function(){
    if(annotate == TRUE){
      list(
        ggplot2::geom_text(
          data = annotation_df,
          mapping = aes(x= w_opt,
                        y= d_max,
                        label = .data$d_max_label_text),
          nudge_y = 0.03,
          size = 7/.pt,
          parse = TRUE
        ),
        ggplot2::geom_text(
          data = annotation_df,
          mapping = aes(x= w_opt,
                        y= d_max,
                        label = .data$w_opt_label_text),
          nudge_y = 0.01,
          size = 7/.pt,
          parse = TRUE
        )
      )
    }
  }

  # make geom for curve
  geom_proctor_curve <- function(){

      ggplot2::geom_smooth(
      data = df,
      ggplot2::aes(x= .data$water_content,
                   y= .data$dry_density,
                   color = {{identifier}}),
      method= "lm",
      formula = y~splines::ns(x, 3),
      se = FALSE,
      ...)

  }

  # make geom for points
  geom_proctor_points <- function(){

    ggplot2::geom_point(
      data = df,
      ggplot2::aes(x= .data$water_content,
                   y= .data$dry_density,
                   color = {{identifier}}),
      size=1.5,
      alpha = 0.8,
      ...)
  }

  # make data frame and tat_function with label for 100 % saturation line

  sat_curves_data <- df %>%
    dplyr::mutate(w_rank = dplyr::row_number(.data$water_content)) %>%
    dplyr::filter(.data$w_rank > 2) %>%
    dplyr::mutate(
      w_grid = modelr::seq_range(x= .data$water_content,
                                 n = length(.data$water_content),
                                 expand = 0.25),
      sat_100_density =
        1 / ( (1/.data$Gs) + (.data$w_grid/0.9978) + ((.data$w_grid/0.9978)*(1-1)/(1) ) ),
      sat_90_density=
        1 / ( (1/.data$Gs) + (.data$w_grid/0.9978) + ((.data$w_grid/0.9978)*(1-0.9)/(0.9)) )
    )

  geom_sat_100 <- function(){


    if(sat_100 == TRUE) {
      list(
        ggplot2::stat_function(
          fun = ~1 / ( (1/unique(df$Gs)) + (.x/0.9978) + ((.x/0.9978)*(1-1)/(1) ) ),
          linetype= 'dotted',
          xlim = c(min(sat_curves_data$w_grid),
                   max(sat_curves_data$w_grid) ),
          size = 0.25,
          color= 'grey40'
          ),
        ggplot2::annotate(
          "text",
          label = "S[e]==1",
          x= max(sat_curves_data$w_grid),
          y= min(sat_curves_data$sat_100_density + 0.02),
          size= 6/.pt,
          color= 'grey40',
          parse= T,
          angle = -55
        )
      )
    }
  }

  geom_sat_90 <- function(){


    if(sat_90 == TRUE) {
      list(
        ggplot2::stat_function(
          fun = ~1 / ( (1/unique(df$Gs)) + (.x/0.9978) + ((.x/0.9978)*(1-0.9)/(0.9) ) ),
          linetype= 'dotted',
          xlim = c(min(sat_curves_data$w_grid),
                   max(sat_curves_data$w_grid) ),
          size = 0.25,
          color= 'grey60'
        ),
        ggplot2::annotate(
          "text",
          label = "S[e]==0.9",
          x= max(sat_curves_data$w_grid),
          y= min(sat_curves_data$sat_90_density + 0.02),
          size= 6/.pt,
          color= 'grey60',
          parse= T,
          angle= -55
        )
      )
    }
  }

  # make plot

  proctor_plot <- ggplot2::ggplot(data = df)+
    geom_sat_100()+
    geom_sat_90()+
    geom_proctor_annotation()+
    geom_proctor_curve()+
    geom_proctor_points()+
    ggplot2::scale_x_continuous(
      name= bquote("Water content, % g g" ^ -1),
      labels = scales::label_percent(accuracy=1, suffix = ""),
      breaks = scales::breaks_width(width = 0.02, offset = 0),
      expand = expansion(mult=c(0,0),
                         add= c(0.01, 0.01))) +
    ggplot2::scale_y_continuous(
      name = bquote("Dry density, Mg m"^-3),
      labels = scales::label_number(accuracy = 0.01),
      breaks= scales::breaks_width(width = 0.05, offset = 0),
      expand= expansion(mult= c(0,0),
                        add= c(0.05, 0.05)))+
    ggplot2::coord_fixed(ratio = 0.32) +
    ggplot2::scale_color_brewer(palette = "Set1") +
    cowplot::theme_cowplot() +
    cowplot::background_grid(minor="xy")

  return(proctor_plot)

} # end of function
