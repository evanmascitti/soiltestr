## code to prepare `psa_protocols` dataset goes here
library(tidyverse, quietly = T)

# make a tibble containing the summaries of each protocol
# note that protocl_ID is a character type even though I am using numbers
# this is to allow the protocols to each be named list objects
# because object names cannot be numeric type. I guess ultimately
# it actually doesn't matter because you have to use backticks anyway,
# but R takes care of this when bringing them into a lists
# Anyway, I think it is too late to change this now, and not that important.

protocol_summaries <- readr::read_csv(here::here("inst/lab_protocols/particle_size_analysis/psa-methods-terse.csv"),
                col_types = cols(.default = "c"), na= "-")

##########

# join to more detailed descriptions

# find the relevant files containing detailed descriptions
# the references file needs some additional massaging since it will be a
# list-column, so don't include in the list

details_files <-list.files(
  path = here::here("inst/lab_protocols/particle_size_analysis"),
  pattern = "expanded-details[.]csv$", full.names = T) %>%
  str_subset(pattern = "references", negate = T)

references_file <- list.files(
  path = here::here("inst/lab_protocols/particle_size_analysis"),
  pattern = "expanded-details[.]csv$", full.names = T) %>%
  str_subset(pattern = "references", negate = F)

clean_file_name <- function(x){
  basename(x) %>%
    stringr::str_remove(string = ., '_expanded-details[.]csv$') %>%
    stringr::str_remove(., 'psa[_-]') %>%
    stringr::str_replace_all(pattern = "-", replacement = "_")
}

# read references file and nest so it can be joined
references_table <- references_file %>%
  read_csv(col_types = cols(.default = "c")) %>%
  group_by(protocol_ID) %>%
  nest(references = reference) %>%
  ungroup()%>%
  mutate(references = map(references, ~.$reference))


details_list <- details_files %>%
  purrr::set_names(clean_file_name(.)) %>%
  purrr::map(readr::read_csv, col_types = readr::cols(.default = 'c'))


# couldn't figure out how to do this with tidyverse `reduce`, etc
# so just resorted to a good-old-fashioned loop
# but then I figured it out.....the key is the .init argument
# and the fact that some of the column names were alsn not identical
# saving here but commenting out
#
# sum_df <- protocol_summaries
#
# for (i in seq_along(details_list)) {
#
#   sum_df <- left_join(sum_df, details_list[[i]])
#
# }


# Here's the better way to do it: in just two lines of code, all the data frames
# are joined and then the '_details' string is removed the third line and fourth
# lines joins with the references table to add a list-column and then reduce
# this column by a level to make it a character vector instead of a list then
# the variables are arranged to a more logical order
# Then, nest the pre-treatments into a list column.
# Finally, make it into a simple list rather than a tibble, set the names of
# each list element and and name the object for export

protocol_details <- reduce(details_list, left_join, .init = protocol_summaries) %>%
  select(protocol_ID, contains("details")) %>%
  rename_with(.cols = contains('details'), .fn = ~str_remove(., '_details')) %>%
  left_join(references_table) %>%
  relocate(c(protocol_ID, fines_method, coarse_method, g_sample,
             mechanical_dispersion, wash_thru_270_time, OM_removal,
             carbonate_removal, iron_oxide_removal, references, everything())) %>%
  group_by(across(-c(OM_removal:iron_oxide_removal))) %>%
  nest(extra_pretreatments = c(OM_removal:iron_oxide_removal)) %>%
  ungroup()

# Break the sieve sizes and fines sizes into a list-column of numeric vectors
# this will allow the datasheets function to
# look up what the particle diameters should be for the given protocol ID.

# I can't figure out how to do this with mutate and across so just
# going old-school and modifying in place


# was trying to make a function to do it
# make_diameters_listcol <- function(x){
#   stringr::str_split(x, pattern = "-") %>%
#   as.numeric()
# }


protocol_details$fines_diameters_sampled <-  map(protocol_details$fines_diameters_sampled, stringr::str_split, pattern = "-") %>%
  flatten() %>%
  map(as.numeric)


protocol_details$coarse_diameters_sampled <-  map(protocol_details$coarse_diameters_sampled, stringr::str_split, pattern = "-") %>%
  flatten() %>%
  map(as.numeric)

# now over-write the columns that contain the number of sizes sampled; I want them to stay in the
# order that they already are but to use the data from the actual microns rather than having
# to manually type it each time I adda new protocol....basically this makes the column with the number
# in the csv file moot but that's OK for now

protocol_details$n_fines_diameters_sampled <- map_dbl(protocol_details$fines_diameters_sampled, length)

protocol_details$n_coarse_diameters_sampled <- map_dbl(protocol_details$coarse_diameters_sampled, length)




# put all into one condensed list and name with the protocol ID

(psa_protocols <- protocol_details %>%
  group_by(protocol_ID) %>%
  nest() %>%
  .$data %>%
  set_names(protocol_details$protocol_ID))


# I was going to separately include the abbreviated protocol info
# and the details in a list, but I think the abbreviations are only useful
# for building this data object, and not to a user of the package....
# therefore this data object only exports the polished tibble,
# which contains 2 list-columns and several other columns about the
# process . Should I later wish to re-visit, the code to compile into a larger list with the summary is below:

#
# psa_protocols <- list(
#   summary = protocol_summaries,
#   details = protocols_list
# )


# export list as a data object
usethis::use_data(psa_protocols, overwrite = TRUE)
