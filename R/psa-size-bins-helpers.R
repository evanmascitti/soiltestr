#' Title only gravel (<4000 microns), sand, silt, and clay reported
simple_bins <- function(){

  # assign the needed object from parent frame instead of
  # passing them as arguments

  cumulative_percent_passing <- get("cumulative_percent_passing", envir = parent.frame() )

  # This call should inherit the object from its parent frame; i.e. the psa() environment. One can also look recursively upward by setting inherits= TRUE as below although in this case it did not work for me ...? I guess it is best to explicitly specify the environment to look in anyway,
  # in case any varible names are duplicated


  # cumulative_percent_passing <- get("cumulative_percent_passing", inherits = T )





  size_bins <- cumulative_percent_passing %>%
    tidyr::pivot_wider(names_from = .data$microns,
                       values_from = .data$percent_passing) %>%
    dplyr::mutate(
      gravel = .data$`4000` - .data$`2000`,
      sand = .data$`2000` - .data$`53`,
      silt = .data$`53` - .data$`2`,
      clay = .data$`2`) %>%
    dplyr::select(.data$date:.data$batch_sample_number,
                  .data$gravel:.data$clay) %>%
    dplyr::mutate(
      dplyr::across(
        .cols = .data$gravel:.data$clay,
        .fns = ~.*100))

}


#' Size bins include USGA sieves and pipette sizes of 20, 5, 2, and 0.2 microns
#'
all_bins <- function(){

  # assign cumulative percent passing to local variable based on its value in global environment

  cumulative_percent_passing <- get("cumulative_percent_passing", envir = parent.frame() )


  size_bins <- cumulative_percent_passing %>%
    tidyr::pivot_wider(names_from = .data$microns, values_from = .data$percent_passing) %>%
    dplyr::mutate(
      gravel = .data$`4000` - .data$`2000`,
      very_coarse_sand = .data$`2000` - .data$`1000`,
      coarse_sand = .data$`1000` - .data$`500`,
      medium_sand = .data$`500` - .data$`250`,
      fine_sand = .data$`250` - .data$`150`,
      very_fine_sand = .data$`150` - .data$`53`,
      coarse_silt = .data$`53` - .data$`20`,
      medium_silt = .data$`20` - .data$`5`,
      fine_silt = .data$`5` - .data$`2`,
      coarse_clay = .data$`2` - .data$`0.2`,
      fine_clay = .data$`0.2`,
      sand = .data$very_coarse_sand + .data$coarse_sand + .data$medium_sand + .data$fine_sand + .data$very_fine_sand,
      silt = .data$coarse_silt + .data$medium_silt + .data$fine_silt,
      clay = .data$coarse_clay + .data$fine_clay) %>%
    dplyr::select(.data$date:.data$batch_sample_number,
                  .data$gravel:.data$fine_clay) %>%
    dplyr::mutate(
      dplyr::across(
        .cols = .data$gravel:.data$fine_clay,
        .fns = ~.*100))

}
