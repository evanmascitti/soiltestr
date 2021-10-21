x <- psa(
  dir = "./tests/testthat/test-data/psa/protocol19/",
  bouyoucos_cylinder_dims = asi468::bouyoucos_cylinders,
  tin_tares = asi468::tin_tares,
  beaker_tares = asi468::psa_beaker_tares,
  hydrometer_dims = asi468::astm_152H_hydrometers
)



x$averages$simple_bins
x$averages$sub_bins %>% View()
