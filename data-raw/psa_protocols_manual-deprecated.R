# ## code to prepare `psa_protocols` dataset goes here
# library(magrittr)
#
# # remove any existing objects from global environment
# rm(list = ls())
#
# # make a tibble containing the summaries of each protocol
# # note that protocl_id is a character type even though I am using numbers
# # this is to allow the protocols to each be named list objects
# # because object names cannot be numeric type.
#
# protocol_summaries <- readr::read_csv("inst/lab_protocols_metadata/particle_size_analysis/psa-methods-lookup-table.csv",
#                 col_types = "cccccciccc", na= "-")
#
# # make a named list for each protocol containing the appropriate fields
#
# # first write additional some helper functions to keep the fields consistent
#
# #### to construct the pretreatments argument
#
# pretreatment_list <- function(OM_removal = NULL,
#                               carbonate_removal = NULL,
#                               Fe_oxide_removal = NULL){
#
#  if (missing(OM_removal) || missing(carbonate_removal) || missing(Fe_oxide_removal)) {
#
# stop('One or more missing arguments to `pretreatment_list`' ) }
#
#    list(OM_removal = OM_removal,
#         carbonate_removal = carbonate_removal,
#         Fe_oxide_removal = Fe_oxide_removal)
#
#    }
#
# dispersion_method_list <- function(physical_agitation = NULL,
#                                    chemical_dispersant = NULL){
#
#   # browser()
#
#   if (missing(physical_agitation) || missing(chemical_dispersant)) {
#
#     stop('One or more missing arguments to `dispersion_method_list`' ) }
#
#     mget(ls())
# }
#
# references_list <- function(url = NULL, ...){
#
#   if(missing(url)){
#     stop('No url provided') }
#
#   list("See vignettes at <package URL>", ...)
#
# }
#
#
# ### a helper function to generate the actual list
# protcol_structure <- function(protocol_ID,
#                               fines_measurement_method,
#                               coarse_measurement_method,
#                               pretreatments,
#                               dispersion_methods,
#                               references){
#
#  # browser()
#
#    # solution to check for missing arguments, adapted from
#   #https://stackoverflow.com/questions/38758156/r-check-if-any-missing-arguments
#
# # defined <- ls()
# #
# # passed <- names(as.list(match.call())[-1])
# # if (any(!defined %in% passed)) {
# #   stop(paste("missing values for", paste(setdiff(defined, passed), collapse=", ")))
# #     }
#
# # remove the two  objects created for the argument test
#
# # rm(defined, passed)
#
# #### make actual list to return assuming arguments have been specified correctly
#
# # ideally could use `mget()` or `match.arg()` or similar to dynamically
# # asssign all named arguments into the list, instead of typing them??
#
#
# protocol <- mget(ls(sorted = F))
#
# }
#
#
#
# # The above strategy does actually work, and I am proud to have figured it
# # out.....however I am not convinced it is ideal; it might be better to store
# # the data for the protols in a structured system of external files (i.e. csv
# # and text files), then programatically construct the list from those using readLines,
# # etc.....but I can't spend any more time on this today (2021-03-22)
#
#
# protocol_7 <- protcol_structure(
#   protocol_ID = "7",
#   fines_measurement_method = "pipette",
#   coarse_measurement_method = "gravel+sand only",
#   pretreatments = pretreatment_list(
#     OM_removal = NA,
#     carbonate_removal =  NA,
#     Fe_oxide_removal = NA
#   ),
#   dispersion_methods = dispersion_method_list(
#     physical_agitation = "Orbital/milkshake mixer; 5 min mix time",
#     chemical_dispersant = "Sodium hexametaphosphate; 0.5 g/L final concentration"
#   ),
#   references = references_list(
#     url = "See vignettes at <package URL>",
#     "ASTM F-1632-03 (Reapproved 2018) - Standard Test Method for Particle Size Analysis and Sand Shape Grading of Golf Course Putting Green and Sports Field Rootzone Mixes"
#   )
# )
#
#
# # specify the actual info in the protocol objects -------------------------
#
#
# protocol_1 <- protcol_structure(
#   protocol_ID = "1",
#   fines_measurement_method = "25 mL Lowry pipette",
#   coarse_measurement_method = "5 min sieving with Ro-tap",
#   pretreatments = pretreatment_list(
#     OM_removal = "30% hydrogen peroxide w/ heating (see Gee and Or 2006, p. 276)",
#     carbonate_removal = NA,
#     Fe_oxide_removal = NA
#   ),
#   dispersion_methods = dispersion_method_list(
#     physical_agitation = "Orbital/milkshake mixer; 5 min mix time",
#     chemical_dispersant = "Sodium hexametaphosphate; 0.5 g/L final concentration"
#   ),
#   references = references_list(
#     url = "See vignettes at <package URL>",
#     " Gee, G.W. and D. Or, 2002. 'Particle‐size analysis.' in: Methods of soil analysis: Part 4 physical methods. p. 255-293. ",
#     "Thurman, Nelson C. Ciolkosz, Edward Dobos, Robert R., 1994. Penn State Soil Characterization Lab Methods Manual",
#     "Robinson, 1922"
#   )
# )
#
#
# # assign all objeccts in global environment to a new named list
# psa_protocols <- mget(ls())
#
#
# # export list as a data object
# usethis::use_data(psa_protocols, overwrite = TRUE)
