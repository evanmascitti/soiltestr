## code to prepare `sieve_invoking_protocol_IDs` dataset goes here

# note that this code relies on the word "shaken" being detected in the
# protocol details. This is to distinguish the sieving methods from the
# laser diffraction methods, which do not involve shaking.
# Should other methods arise, this may need to be revisited

sieve_invoking_protocol_IDs <- psa_coarse_methods[psa_coarse_methods$coarse_method == "shaken", ]$protocol_ID

usethis::use_data(sieve_invoking_protocol_IDs, overwrite = TRUE, internal = TRUE)
