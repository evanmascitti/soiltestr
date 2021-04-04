#' Divide a character vector of file paths
#'
#' Helper function for [`psa()`]. Returns a list of two character vectors is
#' returned - one for the files common to all psa_protocols and one which has
#' the files unique to the protocol in use for this test
#'
#' @param directory directory holding the files. Inherited from parent call.
#'
#' @return List of length 2
#'
divide_psa_datafiles <- function(directory){

all_datafile_paths <-list.files(
  path = directory,
  pattern = "^psa_.*\\d{4}-\\d{2}-\\d{2}\\.csv$",
  full.names = T)

datafile_names <- stringr::str_remove(
  string = basename(all_datafile_paths),
  pattern = "_\\d{4}-\\d{2}-\\d{2}\\.csv$") %>%
  stringr::str_remove("psa_")

purrr::set_names(all_datafile_paths, datafile_names)

# find any file paths which match any of the patterns
# this approach is not very elegant - it could definitely be done with
# []. %in%, and/or grep or grepl.... however this works and I
# can't waste any more time on it

common_patterns <- tibble::tibble(
  pattern1 = "hygroscopic_corrections",
  pattern2 = "metadata",
  pattern3 = "specimen_masses",
  all_datafile_paths = all_datafile_paths
)

common_file_paths <- common_patterns %>%
  dplyr::filter(
    stringr::str_detect(all_datafile_paths, pattern = pattern1) |
      stringr::str_detect(all_datafile_paths, pattern = pattern2) |
      stringr::str_detect(all_datafile_paths, pattern = pattern3))%>%
  purrr::pluck("all_datafile_paths")

method_specific_datafile_paths <- all_datafile_paths[!all_datafile_paths %in% common_file_paths]

return_list <- list(
  common_file_paths = common_file_paths,
  method_specific_datafile_paths = method_specific_datafile_paths
)

return(return_list)

}

