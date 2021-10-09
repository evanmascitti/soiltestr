#' Compute water addition for an air-dry soil
#'
#' @param x List/data frame/tibble. Must contain columns `OD_mass`, `w_extant`, and `w_desired`. If `NULL` (the default), each vector must be passed in individually.
#' @param OD_mass Double. Oven-dry mass of soil in g or kg
#' @param w_extant Double. Existing water content (decimal).
#' @param w_desired Double. Desired final water content (decimal)
#' @param ... currently ignored
#'
#' @return A tibble with one row and two columns - the air-dry mass of soil to use and the mass of water to add.
#' @export
#'
#' @example inst/examples/w_addition_example.R
#'
w_addition <- function(OD_mass, w_extant, w_desired, x = NULL, ...){

  # browser()

  if(!is.null(x)){

    w_additions_tbl <- purrr::pmap(
      .l = x,
      .f = compute_w_addition) %>%
      dplyr::bind_rows()

    return_tbl <- dplyr::bind_cols(x, w_additions_tbl)

    return(return_tbl)

      }

# if x is NULL, perform calculation on individual vectors

  return_tbl <- compute_w_addition(
    OD_mass = OD_mass,
    w_extant = w_extant,
    w_desired = w_desired
  )
  return(return_tbl)

}


#' Internal vectorized function which calculates water additions. Wrapped by `w_addition()`
#'
#' @inheritParams w_addition
#' @return Tibble
#'
compute_w_addition <- function(..., OD_mass, w_extant, w_desired, x = NULL){

  # browser()

  all_vectors <- list(OD_mass, w_extant, w_desired)

  all_lengths <- unlist(lapply(all_vectors, length))

  non_length_one_lengths <- unique(all_lengths[!all_lengths == 1L])

  if(length(non_length_one_lengths) > 1L){
    stop("Vectors supplied to `OD_mass`, `w_extant`, and `w_desired` must be of equal length.")
  }

  # is_single_value <- all_lengths == 1

  # if any values are not either 1 or the unique length throw an error

  # if all lengths are > 1 and



  # length_check <- lapply(all_lengths, identical, all_lengths[[1]])
  #
  # existing_water <-
  # if(!all(length_check)){
  #   stop("Vectors supplied to `OD_mass`, `w_extant`, and `w_desired` must be of equal length.")
  # }


  existing_water <- OD_mass * w_extant

  final_water <- OD_mass * w_desired

  air_dry_soil_to_use <- OD_mass + existing_water

  water_to_add <- final_water - existing_water

  return_tbl <- tibble::tibble(
    air_dry_soil_to_use = air_dry_soil_to_use,
    water_to_add = water_to_add
  )

  return(return_tbl)

}
