# prepare reference sheet for wetting samples from existing water contents and
# estimated w_opt values (these should be estimated visually and confirmed with
# water content measurements, but are simply typed here for demonstration)
library(tibble)
library(dplyr)
library(tidyr)

w_extant_values <- tibble(
  sample_ID = paste0("mix_0", 1:3),
  w_extant = c(0.048, 0.057, 0.062))

PL_values <- tibble(
  sample_ID = paste0("mix_0", 1:3),
  PL= c(0.095, 0.10, 0.14),
  standard = 0.9*PL,
  modified= standard - 0.03)

prep_sheet <- w_extant_values %>%
  left_join(PL_values) %>%
  select(-PL) %>%
  pivot_longer(cols= c(standard:modified),
               names_to =  'effort',
               values_to = 'est_w_opt')%>%
  select(sample_ID, effort, everything()) %>%
  arrange(desc(effort)) %>%
  proctor_prep(date = Sys.Date())

# write file to disk
generate_proctor_datasheet(prep_sheet, dir= paste0(tempdir(), "/") )

# alternatively, return the data sheet as a tibble
generate_proctor_datasheet(prep_sheet, write = FALSE)

