#' Calculate component masses for a 3-way soil mixture
#'
#' Computations are based on desired sand content and silt-to-clay ratio of the final mixture
#'
#'
#' @param mix_date Character.
#' @param final_OD_kg Numeric. Final oven-dry mass of mixture.
#' @param mix_name Character, if `NULL` a name is constructed as "`sandy_name`-`silty-name`-`clayey_name`-scr-`final_scr`_`final_sand_content`"
#' @param sandy_name Character. Name of sandy soil component.
#' @param silty_name Character. Name of siltiest soil component.
#' @param clayey_name Character. Name of clayey soil component.
#' @param final_sand_pct Numeric (decimal). Desired mass percent of sand-size particles.
#' @param final_scr Numeric. Desired silt-to-clay ratio of final mixture.
#' @param sand_sandy Numeric (decimal). Sand-size content of sandy component.
#' @param silt_sandy Numeric (decimal). Sand-size content of silty component.
#' @param clay_sandy Numeric (decimal). Sand-size content of clayey component.
#' @param sand_silty Numeric (decimal). Silt-size content of sandy component.
#' @param silt_silty Numeric (decimal). Silt-size content of silty component.
#' @param clay_silty Numeric (decimal). Silt-size content of clayey component
#' @param sand_clayey Numeric (decimal). Clay-size content of sandy component.
#' @param silt_clayey Numeric (decimal). Clay-size content of silty component.
#' @param clay_clayey Numeric (decimal). Clay-size content of clayey component
#' @param w_sandy  Numeric (decimal). Water content of sandy component.
#' @param w_silty Numeric (decimal). Water content of sandy component.
#' @param w_clayey Numeric (decimal). Water content of sandy component.
#' @param ... Other arguments passed to methods (currently ignored)
#'
#' @return
#' @export
#'
sand_w_scr_mix_calcs <- function(
  mix_date,
  final_OD_kg,
  sample_name = NULL,
  sandy_name = NULL,
  silty_name = NULL,
  clayey_name = NULL,
  final_sand_pct,
  final_scr,
  sand_sandy,
  silt_sandy,
  clay_sandy,
  sand_silty,
  silt_silty,
  clay_silty,
  sand_clayey,
  silt_clayey,
  clay_clayey,
  w_sandy,
  w_silty,
  w_clayey,
  ...
){

  # browser()

  if(any(final_sand_pct > 1)){
    stop('`final_sand_pct` must be supplied as a decimal. Did you supply a percent?',
         call. = FALSE)
  }

  # the final OD kg for this will never actually be used because
  # the mass of the fine components is re-normalized to the final mixture
  # mass....however here the final_OD_kg argument is specified as _the same_
  # as that for the whole function call....this ensures the mass of
  # fines never ends up negative

  new_fines_components <- scr_mix_calcs(
    final_OD_kg = final_OD_kg,
    scr = final_scr,
    silt_silty = silt_silty,
    clay_silty = clay_silty,
    silt_clayey = silt_clayey,
    clay_clayey = clay_clayey,
    w_silty = w_silty,
    w_clayey = w_clayey
    )

  # construct name for the final mixture if not supplied by user

  if(is.null(sample_name) & all(is.null(c(sandy_name, silty_name, clayey_name)))){
    stop("No names provided for mixture components. Please supply either a sample name or names for each of the 3 components.")
  }

  sample_name <- sample_name %||% paste(
    sandy_name,
    paste0(
      silty_name,
      "_",
      clayey_name,
      "_scr-",
      final_scr),
    paste0(
      as.character(round(100 * final_sand_pct), digits = 0),
      '-pct-sand'),
    sep = "_")


   # browser()


  sand_w_new_fines_mix_components <- mix_calcs(
    mix_date = mix_date,
    sample_name = sample_name,
    sandy_name = sandy_name,
    clayey_name = clayey_name,
    final_sand_pct = final_sand_pct,
    final_OD_kg = final_OD_kg,
    sand_sandy = sand_sandy,
    sand_clayey = new_fines_components$final_OD_sand_pct,
    w_sandy = w_sandy,
    w_clayey = new_fines_components$final_mix_w,
    format_names = FALSE)

  # from the new fines list and the required mass of fines, compute the air-dry
  # masses of silty and clayey soils
  # this is done in a vectorized i.e. row-wise way. Do NOT use the `sum` function as this will sum the whole vector, which will be correct if only
  # one mixture is being made, but will NOT be correct if multiple mixes are held in the tibble.

  air_dry_fines_mass <- unlist(sand_w_new_fines_mix_components$kg_air_dry_clay_component)

  air_dry_silty_mass <- air_dry_fines_mass * (new_fines_components$air_dry_component_masses$air_dry_kg_silty / (new_fines_components$air_dry_component_masses$air_dry_kg_silty + new_fines_components$air_dry_component_masses$air_dry_kg_clayey))

  air_dry_clayey_mass <- air_dry_fines_mass * (new_fines_components$air_dry_component_masses$air_dry_kg_clayey / (new_fines_components$air_dry_component_masses$air_dry_kg_silty + new_fines_components$air_dry_component_masses$air_dry_kg_clayey))

 # also assign a variable for the air-dry mass of sandy soil to use.

  air_dry_sandy_mass <- sand_w_new_fines_mix_components$kg_air_dry_sand_component

  # compute water content of new mixture

  # browser()

  ####################

  sandy_water_contributions <- w_sandy * air_dry_sandy_mass

  silty_water_contributions <- w_silty * air_dry_silty_mass

  clayey_water_contributions <- w_clayey * air_dry_clayey_mass

  total_water_present <- sandy_water_contributions + silty_water_contributions + clayey_water_contributions

  total_air_dry_mass <- air_dry_sandy_mass + air_dry_silty_mass + air_dry_clayey_mass

  new_mix_water_contents <- total_water_present / total_air_dry_mass

  return_tbl <- tibble::tibble(
    sample_name = sample_name,
    final_sand_pct = final_sand_pct,
    final_scr = final_scr,
    air_dry_sandy_mass = air_dry_sandy_mass,
    air_dry_silty_mass = air_dry_silty_mass,
    air_dry_clayey_mass = air_dry_clayey_mass,
    final_mix_w = round(new_mix_water_contents, digits = 3)
  ) %>%
    dplyr::mutate(
      dplyr::across(
        .cols = dplyr::matches("air_dry_(sandy|silty|clayey)_mass"),
        .fns = round, digits = 2
      )
    )

  return(return_tbl)


}
