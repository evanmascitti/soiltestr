#' Remove the "g" printed by A&D balance
#'
#' Returns a numeric column after parsing and removing any non-numeric characters
#'
#' @param df
#'
#' @return
#'
clean_g <- function(df){

  df %>%
    dplyr::mutate(
      dplyr::across(
        .cols = dplyr::matches("^tin_(w_wet_sample|w_OD_sample|number)$"),
        .fns = readr::parse_number
      )
    )

}
