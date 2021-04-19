# These functions are used in the final step of a PSA which was done
# with multiple reps for multiple samples.

# It might be slightly overkill but it allowed me to get some more
# practice with a couple advanced **purrr** maneuvers. The modify_if concept
# is awesome.

# In these functions I am basically doing the same transformation on three
# data frames, but because the cumulative % passing is in long format,
# it has to first be transformed to wide format so that I can use across to
# summarize all the numeric columns. Then it has to be transformed back to long.
# format. If I didn't have to avoid summarizing the microns column this
# would be simpler, but I just decided to create two helpers that use predicates
# to only transform the list elements (i.e. data frames) that have a column
# named "microns", which in this case is obviously only one data frame.

# Then I can use a general function to perform the averaging for _all_ the
# data frames in the list before doing the reverse of the process described
# above. That leaves the original list in its same format, but less the
# columns for the batch sample number and replication, as these are inherently
# not part of a summarizing process over replicated samples.

##############################################################################

#' Get cumulative percent passing into long format
#'
#' @param df data frame with microns and cumulative % passing
#'
#' @return data frame in wide format

pivot_cumulative_percent_passing_wider <- function(df) {

  df %>%
    tidyr::pivot_wider(names_from = .data$microns,
                       values_from = .data$percent_passing)
}


#' Get cumulative percent passing back into wide format
#'
#' @param df data frame with microns as columns and cumulative % passing as the
#'   values for each column
#'
#' @return data frame in long format; the reciprocal of
#'   pivot_cumulative_percent_passing_wider
#'
pivot_cumulative_percent_passing_longer <- function(df) {

  df %>%
    tidyr::pivot_longer(cols = -c(.data$date:.data$sample_name),
                        names_to = 'microns',
                        values_to = 'percent_passing') %>%
    dplyr::mutate(microns = as.numeric(.data$microns))

}


#' Lumps replicated samples together and computes the mean values
#'
#' Does the transformation over any numeric columns; data frames are first
#' grouped by the identifiers standard to a psa data file (date, experiment
#' name, etc.)
#'
#' @param df data frame having standard psa structure (i.e. metadata that is
#' included in every file) and one or more numeric columns to summarize
#'
#' @return data frame with all replicate samples averaged to a single value for each variable
#'
summarize_psa <- function(df){

  df %>%
    dplyr::group_by(dplyr::across(.cols = c(.data$date:.data$sample_name,)))%>%
    dplyr::select(-c(.data$replication, .data$batch_sample_number)) %>%
    dplyr::summarise(
      dplyr::across(.col = tidyselect::vars_select_helpers$where(is.numeric),
                    .fns = mean), .groups = 'drop')
}



