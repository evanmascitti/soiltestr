# calculate 2-theta for 001 kaolinite peak
compute_tth(d = 7.18)

# calculate 2-theta for 1st 3 00l kaolinite peaks
kaolinite_args <- tibble::tibble(d = 7.18, n = 1:3)
purrr::pmap_dbl(kaolinite_args, compute_tth)

