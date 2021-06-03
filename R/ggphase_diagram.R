#' @title Plot phases of a sand-clay mix
#'
#' @description Generates a ggplot showing volumetrically accurate
#'   representations of the solid, liquid, and gaseous phases in a sand-clay
#'   mixture. Options to label the phases with or without their volume
#'   fractions.
#' @param sand_pct percent sand by dry mass in the mixture
#' @param clay_pct percent clay (i.e. fine-grained soil) by dry mass in the mixture
#' @param G_sa specific gravity of sandy soil
#' @param G_c specific gravity of clay soil
#' @param dry_density dry bulk density in g/cm^3
#' @param water_content gravimetric water content
#' @param sand_color quoted string containing color name or hex code
#' @param clay_color quoted string containing color name or hex code
#' @param alpha_level alpha value for the shaded box areas
#' @param ambient_temp_c temperature used to compute water volume
#' @param labels show the identity of each phase?
#' @param values show the percent volume occupied by each phase?
#' @param base_family character, passed to various text-drawing arguments
#' @param return_data logical, whether to also return a data frame
#'
#' @return list containing a ggplot object and a tibble of the phase volumes. If return_data = FALSE, the second list element is NULL
#' @export
#'
#' @example inst/examples/ggphase_diagram_example.R
#'
#' @importFrom rlang `%||%`
#'
ggphase_diagram <- function(sand_pct, clay_pct, G_sa, G_c, dry_density,
                     water_content, sand_color = "#c2b280", clay_color = "#643F30", alpha_level = 1/4,
                     ambient_temp_c = 22, labels = TRUE, values = TRUE, base_family = NULL, return_data = TRUE){

  base_family <- base_family %||% 'sans'

  G_w <- soiltestr::h2o_properties_w_temp_c %>%
    dplyr::filter(dplyr::near(.data$water_temp_c, round(ambient_temp_c, 1))) %>%
    .$water_density_Mg_m3 %>%
    .[1]

  phases <- tibble::tibble(
    x = 0.2,
    m_sa = sand_pct,
    m_c = clay_pct,
    Gs = .data$m_sa*G_sa + .data$m_c*G_c,
    w = water_content,
    m_w = .data$w*(.data$m_sa + .data$m_c),
    v_sa_abs = .data$m_sa / G_sa,
    v_c_abs = .data$m_c / G_c,
    v_w_abs = .data$m_w / G_w,
    v_a_abs = ((.data$m_sa + .data$m_c)/dry_density)-((.data$m_sa/G_sa)+(.data$m_c/G_c)+(.data$m_w/G_w)),
    v_tot_abs = sum(.data$v_sa_abs, .data$v_c_abs, .data$v_w_abs, .data$v_a_abs),
    v_sa = .data$v_sa_abs / .data$v_tot_abs,
    v_c = .data$v_c_abs / .data$v_tot_abs,
    v_w = .data$v_w_abs / .data$v_tot_abs,
    v_a = .data$v_a_abs / .data$v_tot_abs,
    v_tot = .data$v_tot_abs / .data$v_tot_abs)

  base_phases_plot <- suppressMessages(
    ggplot2::ggplot(data = phases, ggplot2::aes(x = .data$x))+
      ggplot2::geom_rect(ggplot2::aes(xmin = 0.1, xmax = 0.2, ymin = 0, ymax = .data$v_sa/.data$v_tot),
                         alpha = alpha_level, fill = sand_color, color ='grey60', size = 0.25)+
      ggplot2::geom_rect(ggplot2::aes(xmin = 0.1, xmax = 0.2, ymin = .data$v_sa/.data$v_tot, ymax = sum(.data$v_sa, .data$v_c)/.data$v_tot),
                         alpha = alpha_level, fill = clay_color, color ='grey60', size = 0.25)+
      ggplot2::geom_rect(ggplot2::aes(xmin = 0.1, xmax = 0.2, ymin = sum(.data$v_sa, .data$v_c)/.data$v_tot, ymax = sum(.data$v_sa, .data$v_c, .data$v_w)/.data$v_tot),
                         alpha = alpha_level, fill = "#d4f1f9", color ='grey60', size = 0.25)+
      ggplot2::geom_rect(ggplot2::aes(xmin = 0.1, xmax = 0.2, ymin = sum(.data$v_sa, .data$v_c, .data$v_w)/.data$v_tot, ymax = .data$v_tot),
                         alpha = alpha_level, fill = "transparent", color ='grey60', size = 0.25)+
      ggplot2::coord_fixed(ratio = 0.2, xlim = c(0, 0.35))+
      ggplot2::theme_void(base_family = base_family)
  )

  phases_plot_no_numbers <- suppressMessages(
    base_phases_plot+
      ggplot2::annotate('text', label = bquote("V"["sand"]), x = 0.21, y= (phases$v_sa/2), size = 4, hjust = 0, family = base_family)+
      ggplot2::annotate('text', label = bquote("V"["clay"]), x = 0.21, y= (phases$v_sa + phases$v_c/2), size = 4, hjust = 0, family = base_family)+
      ggplot2::annotate('text', label = bquote("V"["water"]), x = 0.21, y= (phases$v_sa + phases$v_c + phases$v_w/2), size = 4, hjust = 0, family = base_family)+
      ggplot2::annotate('text', label = bquote("V"["air"]), x = 0.21, y= (phases$v_tot - 0.5*phases$v_a), size = 4, hjust = 0, family = base_family)+
      ggplot2::coord_fixed(ratio = 0.2, xlim = c(0, 0.35))+
      ggplot2::theme_void(base_family = base_family)
  )

  phases_plot_w_numbers <- suppressMessages(
    base_phases_plot+
      ggplot2::annotate('text', label = bquote("V"["sand   "]==~.(scales::percent(phases$v_sa))), x = 0.21, y= (phases$v_sa/2),  hjust = 0, family = base_family)+
      ggplot2::annotate('text', label = bquote("V"["clay    "]==~.(scales::percent(phases$v_c))), x = 0.21, y= (phases$v_sa + phases$v_c/2),  hjust = 0, family = base_family)+
      ggplot2::annotate('text', label = bquote("V"["water "]==~.(scales::percent(phases$v_w))), x = 0.21, y= (phases$v_sa + phases$v_c + phases$v_w/2),  hjust = 0, family = base_family)+
      ggplot2::annotate('text', label = bquote("V"["air      "]==~.(scales::percent(phases$v_a))), x = 0.21, y= (phases$v_tot - 0.5*phases$v_a),  hjust = 0, family = base_family)+
      ggplot2::coord_fixed(ratio = 0.2, xlim = c(0, 0.35))+
      ggplot2::theme_void(base_family = base_family)
  )

  if(labels == TRUE & values == TRUE){
    return_plot <- suppressMessages(phases_plot_w_numbers)
  } else{
    return_plot <- suppressMessages(phases_plot_no_numbers)
  }

  if(labels == FALSE)
    return_plot <- base_phases_plot

  if(return_data == TRUE){
    phase_volumes <- phases
  } else {
      phase_volumes <- NULL
    }

  return_list <- list(phase_diagram_plot = return_plot,
                      phase_volumes = phase_volumes)

 # browser()

  return(return_list)
}

