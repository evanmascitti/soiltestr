#' `r lifecycle::badge('maturing')`
#'
#' @title Calculate soil mixture component weights
#'
#'\loadmathjax
#'@description Returns a concise, printer-friendly
#'reference sheet. It is useful when mixing two soils ("sand" and "clay") from
#'an air-dry condition to produce a final mixture having a particular %
#'sand-size particles. See **Details** for more info on the calculations.
#'
#' @param mix_date Date the mixture is being produced in ISO format (YYYY-MM-DD).
#' @param sample_name Character vector of the unique mix identifiers
#' @param sandy_name Unique name assigned to the particular sand material
#' @param clayey_name Unique name assigned to the particular sand material
#' @param final_sand_pct Numeric vector of final sand contents (decimal form)
#'   \code{expt.mix.nums}. It corresponds to the final desired % of sand-size
#'   particles in the soil mixture on an oven-dry mass basis.
#' @param final_OD_kg  desired mass of mixed soil to obtain, on an oven-dry mass
#'   basis (in kilograms). Defaults to 35 kg. This is a good estimate of the
#'   mass needed to perform a standard Proctor test, a modified Proctor test,
#'   and to prepare 3 "cleat-mark" cylinders, with a 10% extra estimate to allow for PSA, Atterberg limits, and a margin for error.
#' @param sand_sandy a numeric vector of length 1 representing the
#'   fraction of the "sand" component which is >53 &mu;m sieve diameter, on an
#'   oven-dry mass basis (decimal form).
#' @param sand_clayey a numeric vector of length 1 representing the
#'   fraction of the "clay" component which is >53 &mu;m sieve diameter, on an
#' oven-dry mass basis (decimal form).
#' @param w_sandy The gravimetric water content of the air-dry "sand" component,
#'   in decimal form.
#' @param w_clayey The gravimetric water content of the air-dry "clay" component,
#'   in decimal form.
#' @param w_final a numeric vector of the same length as `expt_mix_nums`
#'   (if the mixes are to have different water contents), or a single numeric
#'   value (if all mixes are to have the same final water content). Defaults to
#'   0.05 which is the lowest water content typically used in a compaction test.
#'
#'
#'@return A table of values with an appropriate number of
#'  significant figures.
#'
#'@details
#'This function solves a 2-member system of equations, accounting for the
#'hygroscopic water content of each soil and their respective % sand-size
#'particles. The user may choose any desired final % sand, and (so long as the
#'water contents of each soil are known), the final mixture will contain the
#'desired % sand on an oven-dry mass basis.
#'
#'The equation for the air-dry mass of sandy soil is
#'
#' \mjdeqn{m_{sandy~(air-dry)}~=~\frac{S_f~-~S_{clayey}}{S_{sandy}~-~S_{clayey}}~\cdot~(1+w_{sandy})~\cdot~m_{~total~mixture}}{}
#'
#'and the equation for the
#'air-dry mass of clayey soil is
#'
#'\mjdeqn{
#'m_{clayey~(air-dry)}~=~\left\lbrack(1~-~\left(\frac{S_f~-~S_{clayey}}
#'{S_{sandy}~-~S_{clayey}}\right)\right\rbrack~\cdot~
#'(1~+~w_{clayey})~\cdot~m_{~total~mixture~(oven-dry)} }{}
#'
#'
#'@example inst/examples/sand_clay_mix_calcs_example.R
#'
#'@export
#'
#' @importFrom rlang `%||%`
#'

sand_clay_mix_calcs <- function(
  mix_date,
  sample_name = NULL,
  sandy_name,
  clayey_name,
  final_sand_pct,
  final_OD_kg = 35,
  sand_sandy,
  sand_clayey,
  w_sandy,
  w_clayey) {

 #  browser()


  if(any(final_sand_pct > 1)){
    stop('`final_sand_pct` must be supplied as a decimal. Did you supply a percent?',
         call. = FALSE)
  }

  if(any(w_sandy > 1 | w_clayey > 1)){
    stop('Water contents must be supplied as decimals. Did you supply a percent?',
         call. = FALSE)
  }

# check that vectors of length > 1 are of same length
  # currently disabled to allow recycling of vectors having length 1....there
  # is another way to check this but for now I will just let the tibble
  # package take care of it
  # vector_args <- list(
  #   mix_date = mix_date,
  #   sample_name = sample_name,
  #   sandy_name = sandy_name,
  #   clayey_name = clayey_name,
  #   final_sand_pct = final_sand_pct
  #   )
  #
  # vector_lengths <- purrr::map_int(vector_args, length)
  #
  # length_check <- purrr::map2_lgl(vector_lengths, vector_lengths[[1]], identical)

 # if(!all(length_check)){
 #    stop("Lengths of vector arguments are not equal.")
 #  }

  # if sample name not specified, create it from the sand name, clay name, and
  # final sand content

  sample_name <- sample_name %||% paste(sandy_name, clayey_name, as.character(100 * final_sand_pct), sep = "_")


  # browser()

  mix_ref <-   tibble::tibble(
    mix_date = lubridate::as_date(mix_date),
    sample_name = sample_name,
    sandy_name = sandy_name,
    clayey_name = clayey_name,
    final_sand_pct = final_sand_pct,
    final_OD_kg = final_OD_kg,
    OD_sand_size_mass_in_final_mix = final_OD_kg*.data$final_sand_pct,
    OD_non_sand_size_mass_in_final_mix = final_OD_kg - .data$OD_sand_size_mass_in_final_mix,
   kg_OD_sandy_component = ( final_OD_kg * (.data$final_sand_pct - sand_clayey) / (sand_sandy - sand_clayey) ),
    kg_OD_clayey_component = final_OD_kg - .data$kg_OD_sandy_component,
    kg_air_dry_sandy_component = .data$kg_OD_sandy_component*(1+w_sandy),
    kg_air_dry_clayey_component = .data$kg_OD_clayey_component*(1+w_clayey),
    kg_water_already_present = ( (w_sandy * .data$kg_OD_sandy_component) + (w_clayey * .data$kg_OD_clayey_component) ),
    new_mix_w = kg_water_already_present / final_OD_kg,
   kg_air_dry_silty_component = NA_real_) %>%
    dplyr::select(
      mix_date, sample_name, sandy_name, clayey_name, .data$final_sand_pct,
      .data$kg_air_dry_sandy_component,
      .data$kg_air_dry_silty_component,
      .data$kg_air_dry_clayey_component,
      .data$new_mix_w) %>%
    dplyr::mutate(
      final_sand_pct = round(100 * .data$final_sand_pct, digits = 0),
      new_mix_w = round(new_mix_w, digits = 3),
      kg_air_dry_sandy_component = round(.data$kg_air_dry_sandy_component, digits = 2),
      .data$kg_air_dry_silty_component,
      kg_air_dry_clayey_component = round(.data$kg_air_dry_clayey_component, digits = 2))


  return(structure(mix_ref, class = c(class(mix_ref), 'soiltestr_mix_calcs')))

}


#' A helper, later on I will define this as an S3 method
#'
#' Formats the column names for prettier printing in R Markdown documents
#'
#' @param x S3 object of class `soiltestr_mix_calcs`
#'
#' @return
#' @export
#'
format_mix_calcs <- function(x){

  mix_ref <- x %>%
    dplyr::rename(`Mix date`=  mix_date,
                  `Sample name` = sample_name,
                  `Sand name` = sandy_name,
                  `Clay name`= clayey_name,
                  `Final % sand-size` = .data$final_sand_pct,
                  `kg sand component`= .data$kg_air_dry_sandy_component,
                  `kg clay component`= .data$kg_air_dry_clayey_component,
                  `Mix water content` = .data$w_extant)

}
