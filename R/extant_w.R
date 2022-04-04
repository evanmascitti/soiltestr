#' Compute water contents from a .csv file of raw data
#'
#' @param file full path to .csv file
#' @param tin_tare_set character (length 1) identifying the set of tins
#' @param col_name name of the new data frame column containing the gravimetric water content. Alternative choices could include `w_extant` or similar.
#'
#' @return data frame containing added column of water contents
#' @export
#'
extant_w <- function(file, tin_tares = NULL, col_name = "water_content"){


  tin_tares <- tin_tares %||% getOption('soiltestr.tin_tares') %||% internal_data$equipment_instructions("tin_tares")


  x <- readr::read_csv(
    file = file,
    col_types = readr::cols(
      tin_tare_set = readr::col_character()),
    na = "-"
      ) %>%
    dplyr::left_join(tin_tares, by = c("tin_tare_set", "tin_number")) %>%
    add_w() %>%
    dplyr::select(date, sample_name, water_content)

  if(col_name  != 'water_content'){


    old_col_names <- colnames(x)

    new_col_names <- stringr::str_replace(colnames(x), "water_content", col_name)

    colnames(x) <- new_col_names


  }

  return(x)

}


