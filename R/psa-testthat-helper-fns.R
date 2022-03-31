
#' A basic helper to construct a psa object from the ID, **ONLY** for use in automated tests
#'
#' @param protocol_ID
#'
#' @return psa object
make_test_psa <- function(protocol_ID){

  # find correct directory and construct psa object

  directory_to_search <- list.dirs(
    path = here::here(
      "tests", "testthat", "test-data", "psa",
      paste0("protocol", protocol_ID)),
    full.names = TRUE,
    recursive = FALSE
  )

  psa_object <- psa(dir = directory_to_search)

}



#' Verify the contents of a psa object
#'
#' @param protocol_ID protocol used for test, see [`psa_protocols`]
#'
#' @return a series of results from calls to testthat functions
test_psa_sums <- function(psa_object){

  psa_object <- psa_object

# compute overall sums ----------------------------------------------------

# check that sand, silt, and clay sum to 100 % for simple bins
  # or are NULL if not computed

  simple_bins_sums <- psa_summation(psa_object = psa_object,
                                    type = "simple")


  sub_bins_sums <- psa_summation(psa_object= psa_object,
                                 type = "sub")


  }


# some helpers


# sum to 100% or NULL -----------------------------------------------------


#' Check totals of bins

#' @param psa_object a psa object
#' @param bins_type one of "simple" or "sub"
#'
#' @return Logical. Checks that the bins either sum to 100% or are NULL (if not computed)

psa_summation <- function(psa_object, bins_type){

  browser()

bins_to_check <- psa_object[[paste0(bins_type, "_bins")]]

  if(bins_type == "sub" && is.null(bins_to_check)){
    return(TRUE)
  } else{

    sums <-  bins_to_check %>%
    dplyr::select((where(is.numeric))) %>%
    dplyr::mutate(
      dplyr::across(
        .cols = c(replication, batch_sample_number),
        .fns = as.character)) %>%
    tidyr::pivot_longer(
      cols = where(is.numeric),
      values_to =  'percent_in_bin') %>%
    dplyr::group_by(replication, batch_sample_number) %>%
    dplyr::summarize(percent_in_bin = round(sum(percent_in_bin), 0),
                     .groups = 'drop') %>%
    purrr::pluck("percent_in_bin")

  sums_check <- identical(sums, rep(100, length(sums)))

  return(sums_check)

  }


  }


#' Returns the expected names of a psa object
#'
#' @param psa_object
#'
#' @return character vector of length 7
psa_names_check <- function(psa_object) {

  # check names of psa object

  return(c("cumulative_percent_passing",
           "simple_bins",
           "sub_bins",
           "method_metadata",
           "pretreatment_loss",
           "averages",
           "psd_plots")
  )

}
