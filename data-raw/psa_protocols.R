## code to prepare `psa_protocols` dataset goes here
library(magrittr)

# remove any existing objects from global environment
rm(list = ls())

# make a named list for eah protocol containing the appropriate fields

"1" <- list(
    fines_measurement_method = c("25 mL Lowry pipette"),
    coarse_measurement_method = c("5 min sieving with Ro-tap"),
    pretreatments_performed = list(OM_removal = "30% hydrogen peroxide",
                                   carbonate_removal = NA,
                                   Fe_oxide_removal = NA),
    dispersion_method = list(
      physical_agitation = "Orbital/milkshake mixer, 5 min mix time",
      chemical_dispersant = "Sodium hexametaphosphate 0.5 g/L"
    ),
    references = list("See vignettes at <package URL>",
      " Gee, G.W. and D. Or, 2002. 'Particle‐size analysis.' in: Methods of soil analysis: Part 4 physical methods. p. 255-293. ",
      "Thurman, Nelson C. Ciolkosz, Edward Dobos, Robert R., 1994. Penn State Soil Characterization Lab Methods Manual",
"Robinson, 1922")
)

# assign all objets in global environment to a new named list
psa_protocols <- mget(ls())


# export list as a data object
usethis::use_data(psa_protocols, overwrite = TRUE)
