# a single sample
 compaction_aliquots(soil_ID = 'soilB',
                            w_extant = 0.05,
                            est_w_opt = 0.09)


# Use purrr to run this function for multiple soils and combine into a single
# tibble; each column should be a vector with one value per soil.
 mixes <- tibble::tibble(
  soil_ID = paste0("soil_0", c(1:3)),
  w_extant= c(0.08, 0.077, 0.089),
  est_w_opt = c(0.11, 0.105, 0.121)
)

purrr::pmap(.l = mixes,
     .f= compaction_aliquots) %>%
  purrr::reduce(rbind)


